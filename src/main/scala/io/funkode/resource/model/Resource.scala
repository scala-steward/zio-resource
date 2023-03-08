/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.annotation.{switch, tailrec}
import scala.collection.mutable.Stack
import scala.compiletime.*
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

case class ResourceLink(urn: Urn, rel: String, attributes: Map[String, String] = Map.empty)
type ResourceLinks = Map[String, ResourceLink]

case class Resource(
    urn: Urn,
    body: ByteResourceStream,
    format: ResourceFormat = ResourceFormat.Json,
    etag: Option[Etag] = None,
    links: ResourceLinks = Map.empty
)

object Resource:

  case class Of[R](
      urn: Urn,
      body: IO[ResourceError, R],
      etag: Option[Etag] = None,
      links: ResourceLinks = Map.empty
  )

  def fromJsonString(
      urn: Urn,
      bodyString: String,
      format: ResourceFormat = ResourceFormat.Json,
      etag: Option[Etag] = None,
      links: ResourceLinks = Map.empty
  ): Resource = Resource(urn, ZStream.fromIterable(bodyString.getBytes), format, etag, links)

  def fromCaseClass[R](
      urn: Urn,
      typedBody: R,
      etag: Option[Etag] = None,
      links: ResourceLinks = Map.empty
  ) = Resource.Of(urn, ZIO.succeed(typedBody), etag, links)

  def fromAddressableClass[R: Resource.Addressable](
      typedBody: R,
      etag: Option[Etag] = None,
      links: ResourceLinks = Map.empty
  ) = fromCaseClass(typedBody.urn, typedBody, etag, links)

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

  extension (inline resource: Resource)
    inline def of[R]: Resource.Of[R] =
      summonFrom {
        case given JsonDecoder[R] =>
          val parsedBody =
            JsonDecoder[R].decodeJsonStreamInput(resource.body).catchAll { case t: Throwable =>
              ZIO.fail(ResourceError.SerializationError("not able to deserialize resource body", Some(t)))
            }

          Resource.Of[R](resource.urn, parsedBody, resource.etag, resource.links)
        case _ => error("Missing Decoder for type" + codeOf(erasedValue[R]))

      }

    inline def normalizeWithModel[R: Mirror.Of]: ResourceStream[Any] =
      val model = DeriveResourceModel.gen[R]

      for
        collectionModel <-
          ZStream.fromZIO(
            ZIO
              .fromOption(model.collectionForUrn(resource.urn))
              .orElseFail(
                ResourceError.NormalizationError(s"Can't find collection for resource urn ${resource.urn}")
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

  given Show[Resource] = new Show[Resource]:
    def show(r: Resource): String =
      s"""Resource(${r.urn}, ${r.format}${r.etag.map(e => ", etag: \"" + e + "\"").getOrElse("")})"""

  trait Addressable[R]:

    def resourceNid: String
    def resourceNss(r: R): String
    def resourceUrn(r: R): Urn = Urn.parse(s"urn:$resourceNid:${resourceNss(r)}")

    extension (r: R)
      def urn: Urn = resourceUrn(r)
      def asResource: Resource.Of[R] = Resource.fromAddressableClass(r)(using this)

    given addressableToResource(using Resource.Addressable[R]): Conversion[R, Resource.Of[R]] =
      _.asResource
