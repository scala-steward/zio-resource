/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.annotation.{switch, tailrec}
import scala.collection.mutable.Stack
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

import cats.Show
import cats.syntax.show.toShow
import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
import zio.json.JsonDecoder.JsonError
import zio.json.ast.Json
import zio.json.ast.Json.Num
import zio.json.internal.*
import zio.schema.*
import zio.schema.meta.MetaSchema
import zio.stream.*

type ResourceStream[R] = Stream[ResourceError, R]
type ByteResourceStream = ResourceStream[Byte]

enum ResourceFormat:
  case Json

opaque type Etag = String
object Etag:
  def apply(etag: String): Etag = etag
  extension (etag: Etag) def unwrap: String = etag

private def withId(urn: Option[Urn]): String = urn.map("with id " + _).getOrElse(s"(with no id)")

enum ResourceError(msg: String, cause: Option[Throwable] = None) extends Throwable(msg, cause.orNull):
  case NotFoundError(urn: Option[Urn], cause: Option[Throwable])
      extends ResourceError(s"Resource ${withId(urn)} not found", cause)
  case SerializationError(msg: String, cause: Option[Throwable] = None) extends ResourceError(msg, cause)
  case NormalizationError(msg: String, cause: Option[Throwable] = None) extends ResourceError(msg, cause)
  case FormatError(msg: String, cause: Option[Throwable] = None)
      extends ResourceError(s"Format not supported: $msg", cause)
  case UnderlinedError(cause: Throwable) extends ResourceError("Non controlled error", Some(cause))

case class ResourceLink(urn: Urn, rel: String, attributes: Map[String, String] = Map.empty)
type ResourceLinks = Map[String, ResourceLink]

case class Resource(
    id: Urn,
    body: ByteResourceStream,
    format: ResourceFormat = ResourceFormat.Json,
    etag: Option[Etag] = None,
    links: ResourceLinks = Map.empty
)

object Resource:

  def fromString(
      id: Urn,
      bodyString: String,
      format: ResourceFormat = ResourceFormat.Json,
      etag: Option[Etag] = None,
      links: ResourceLinks = Map.empty
  ): Resource = Resource(id, ZStream.fromIterable(bodyString.getBytes), format, etag, links)

  enum JsonParsingPhase:
    case Root
    case Array
    case Object

  @tailrec
  def cleanStack(
      trace: List[JsonError],
      in: RetractReader,
      stack: Stack[JsonParsingPhase]
  ): Stack[JsonParsingPhase] =
    if stack.nonEmpty then
      stack.pop match
        case JsonParsingPhase.Root =>
          if Lexer.nextField(trace, in) then
            stack.push(JsonParsingPhase.Root)
            stack
          else stack
        case JsonParsingPhase.Array =>
          if Lexer.nextArrayElement(trace, in) then
            stack.push(JsonParsingPhase.Array)
            stack
          else cleanStack(trace, in, stack)
        case JsonParsingPhase.Object =>
          if Lexer.nextField(trace, in) then
            stack.push(JsonParsingPhase.Object)
            stack
          else cleanStack(trace, in, stack)
    else stack

  def normalizeJson(in: RetractReader): ResourceStream[Json] =
    val trace: List[JsonError] = Nil
    val K: JsonFieldDecoder[String] = JsonFieldDecoder.string
    val V: JsonDecoder[Json] = Json.decoder

    Lexer.char(trace, in, '{')

    ZStream.paginateZIO[Any, ResourceError, Json, Stack[JsonParsingPhase]](Stack(JsonParsingPhase.Root)) {
      parsing =>
        ZIO
          .attemptBlocking {
            val builder = Map.newBuilder[String, Json]

            val currentlyParsing = parsing.top

            // we assume arrays of objects only
            if currentlyParsing == JsonParsingPhase.Array then
              Lexer.char(trace, in, '{')
              parsing.push(JsonParsingPhase.Object)

            var state: (Json, Option[Stack[JsonParsingPhase]]) = (Json.Null, None)
            if Lexer.firstField(trace, in) then

              var continue: Boolean = true

              while continue do
                val field = Lexer.string(trace, in).toString
                val trace_ = JsonError.ObjectAccess(field) :: trace

                Lexer.char(trace_, in, ':')

                val c = in.nextNonWhitespace()
                in.retract()
                (c: @switch) match
                  case '{' =>
                    Lexer.char(trace_, in, '{')
                    state =
                      Json.Obj.apply(builder.result().toList*) -> Some(parsing.push(JsonParsingPhase.Object))
                    continue = state._1 == Json.Obj()
                  case '[' =>
                    Lexer.char(trace_, in, '[')
                    state =
                      Json.Obj.apply(builder.result().toList*) -> Some(parsing.push(JsonParsingPhase.Array))
                    continue = state._1 == Json.Obj()
                  case _ =>
                    val value = V.unsafeDecode(trace_, in)
                    builder += ((K.unsafeDecodeField(trace_, field), value))

                    if !Lexer.nextField(trace, in) then

                      parsing.pop
                      continue = false

                      val json = Json.Obj(builder.result().toList*)
                      val stack = cleanStack(trace, in, parsing)
                      state = json -> (if stack.nonEmpty then Some(stack) else None)

            state
          }
          .catchAll { case t: Throwable =>
            println(s"Error with streaming ${trace.length}")
            trace.foreach(println)
            t.printStackTrace()
            ZIO.fail(ResourceError.NormalizationError("Error reading stream", Some(t)))
          }
    }

  extension (resource: Resource)
    def of[R: JsonDecoder]: Resource.Of[R] = fromRawResourceToTypedResource.apply(resource)

    inline def normalizeWithModel[R: Mirror.Of]: ResourceStream[Any] =
      val model = DeriveResourceModel.gen[R]

      for
        collectionModel <-
          ZStream.fromZIO(
            ZIO
              .fromOption(model.collectionForUrn(resource.id))
              .orElseFail(
                ResourceError.NormalizationError(s"Can't find collection for resource urn ${resource.id}")
              )
          )
        in <- ZStream.scoped {
          resource.body.toInputStream
            .flatMap(is =>
              ZIO
                .fromAutoCloseable(ZIO.succeed(new java.io.InputStreamReader(is)))
                .map(isr => new zio.json.internal.WithRetractReader(isr))
            )
        }
        fullDoc <- normalizeJson(in)
      yield fullDoc

  case class Of[R](
      id: Urn,
      body: IO[ResourceError, R],
      etag: Option[Etag] = None,
      links: ResourceLinks = Map.empty
  )

  def fromCaseClass[R](
      id: Urn,
      typedBody: R,
      etag: Option[Etag] = None,
      links: ResourceLinks = Map.empty
  ) = Resource.Of(id, ZIO.succeed(typedBody), etag, links)

  given fromRawResourceToTypedResource[R](using JsonDecoder[R]): Conversion[Resource, Resource.Of[R]] =
    resource =>
      val parsedBody = JsonDecoder[R].decodeJsonStreamInput(resource.body).catchAll { case t: Throwable =>
        ZIO.fail(ResourceError.SerializationError("not able to deserialize resource body", Some(t)))
      }

      Resource.Of[R](resource.id, parsedBody, resource.etag, resource.links)

  given Show[Resource] = new Show[Resource]:
    def show(r: Resource): String =
      s"""Resource(${r.id}, ${r.format}${r.etag.map(e => ", etag: \"" + e + "\"").getOrElse("")})"""

  trait Typed[R]:
    self =>

    def resourceCollection: String
    def resourceId(r: R): String
    def resourceUrn(r: R): Urn = Urn.parse(s"urn:$resourceCollection:${resourceId(r)}")
    def resourceWithId(r: R)(newId: String): R

    extension (r: R)
      def collection: String = self.resourceCollection
      def resId: String = resourceId(r)
      def urn: Urn = resourceUrn(r)
      def withId(id: String): R = resourceWithId(r)(id)
