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
