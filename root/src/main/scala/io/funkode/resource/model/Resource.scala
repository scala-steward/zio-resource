/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.compiletime.*

import cats.Show
import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
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

trait Resource:
  def urn: Urn
  def body: ByteResourceStream
  def format: ResourceFormat
  def etag: Option[Etag]
  def links: ResourceLinks

  override def toString: _root_.java.lang.String =
    s"Resource(urn: $urn, format: $format, etag: $etag)"

object Resource:

  case class Of[R](
      urn: Urn,
      body: IO[ResourceError, R],
      etag: Option[Etag] = None,
      links: ResourceLinks = Map.empty
  )

  def empty(urn: Urn, resourceEtag: Option[Etag] = None, resourceLinks: ResourceLinks = Map.empty): Resource =
    fromJsonStream(urn, ZStream.empty, resourceEtag, resourceLinks)

  def fromJsonStream(
      resourceUrn: Urn,
      bodyStream: Stream[Throwable, Byte],
      resourceEtag: Option[Etag] = None,
      resourceLinks: ResourceLinks = Map.empty
  ): Resource = new Resource:
    def urn: Urn = resourceUrn
    def body: ByteResourceStream =
      bodyStream.mapError:
        case e: ResourceError => e
        case t: Throwable     => ResourceError.SerializationError("Error reading resource body", Some(t))
    def format: ResourceFormat = ResourceFormat.Json
    def etag: Option[Etag] = resourceEtag
    def links: ResourceLinks = resourceLinks

  def fromJsonString(
      urn: Urn,
      bodyString: String,
      etag: Option[Etag] = None,
      links: ResourceLinks = Map.empty
  ): Resource = Resource.fromJsonStream(urn, ZStream.fromIterable(bodyString.getBytes), etag, links)

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
      summonFrom:
        case given JsonDecoder[R] =>
          val parsedBody =
            JsonDecoder[R]
              .decodeJsonStreamInput(resource.body)
              .mapError:
                case e: ResourceError => e
                case t: Throwable =>
                  ResourceError.SerializationError("Not able to deserialize resource body", Some(t))

          Resource.Of[R](resource.urn, parsedBody, resource.etag, resource.links)
        case _ => error("Missing Decoder for type" + codeOf(erasedValue[R]))

  extension [R: Resource.Addressable](inline typedResource: Resource.Of[R])
    inline def asJsonResource: Resource =
      val bodyStream = for
        bodyTypedStream <- ZStream.fromZIO(typedResource.body)
        jsonBodyStream <- summonInline[JsonEncoder[R]]
          .encodeJsonStream(bodyTypedStream)
          .map(_.toByte)
          .mapError(e => ResourceError.SerializationError.apply("error serializing json", Option(e)))
      yield jsonBodyStream
      Resource.fromJsonStream(typedResource.urn, bodyStream)

  given Show[Resource] = new Show[Resource]:
    def show(r: Resource): String =
      s"""Resource(${r.urn}, ${r.format}${r.etag.map(e => ", etag: \"" + e + "\"").getOrElse("")})"""

  trait Addressable[R]:

    self =>

    def resourceNid: String
    def resourceNss(r: R): String
    def resourceUrn(r: R): Urn = Urn.parse(s"urn:$resourceNid:${resourceNss(r)}")

    extension (r: R)
      def urn: Urn = resourceUrn(r)
      def asResource: Resource.Of[R] = Resource.fromAddressableClass(r)(using self)
      inline def asJsonResource: Resource = r.asResource.asJsonResource(using self)

    given addressableToResource(using Resource.Addressable[R]): Conversion[R, Resource.Of[R]] =
      _.asResource
