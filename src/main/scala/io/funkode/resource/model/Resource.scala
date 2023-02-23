/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.quoted.{Expr, Quotes, Type}

import cats.Show
import cats.syntax.show.toShow
import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
import zio.json.ast.Json
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

  extension (r: Resource) def of[R: JsonDecoder]: Resource.Of[R] = fromRawResourceToTypedResource.apply(r)

  case class Of[R](
      id: Urn,
      body: IO[ResourceError, R],
      etag: Option[Etag] = None,
      links: ResourceLinks = Map.empty
  )

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
