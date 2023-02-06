/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.quoted.{Expr, Quotes, Type}

import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.schema.meta.MetaSchema
import zio.stream.*

type ResourceCall[R] = IO[ResourceError, R]

opaque type Etag = String
object Etag:
  def apply(etag: String): Etag = etag
  extension (etag: Etag) def unwrap: String = etag

enum ResourceError(msg: String, cause: Option[Throwable] = None) extends Throwable(msg, cause.orNull):
  case NotFoundError(urn: Option[Urn])
      extends ResourceError(s"""Resource ${urn.map("with id " + _).getOrElse("(with no urn)")} not found""")
  case SerializationError(msg: String, cause: Option[Throwable] = None) extends ResourceError(msg, cause)
  case FormatError(msg: String, cause: Option[Throwable] = None)
      extends ResourceError(s"Format not supported: $msg", cause)
  case UnderlinedError(cause: Throwable) extends ResourceError("Non controlled error", Some(cause))

case class ResourceLink(urn: Urn, rel: String, attributes: Map[String, String] = Map.empty)
type ResourceLinks = Map[String, ResourceLink]

trait Resource[Encoder[_], Decoder[_], Body]:

  def id: Urn

  def body: Body
  def deserialize[R: Decoder]: ResourceCall[R]

  def etag: Option[Etag]

  def links: ResourceLinks = Map.empty

object Resource:

  trait Identifiable[R]:
    self =>

    def nid: String
    def resourceNss(r: R): String
    def resourceUrn(r: R): Urn = Urn.parse(s"urn:$nid:${resourceNss(r)}")
    def resourceWithNss(r: R)(newNss: String): R

    extension (r: R)
      def nid: String = self.nid
      def nss: String = resourceNss(r)
      def urn: Urn = resourceUrn(r)
      def withNss(nss: String): R = resourceWithNss(r)(nss)

trait ResourceCollection[Encoder[_], Decoder[_], Body]:

  def currentPage: Int
  def pageSize: Int

  def hasNext(): Boolean
  def next(): ResourceCall[List[Resource[Encoder, Decoder, Body]]]

type JsonResource = Resource[JsonEncoder, JsonDecoder, Json]
