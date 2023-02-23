/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package outbound

import scala.compiletime.*
import scala.quoted.*

import io.lemonlabs.uri.Urn
import io.netty.util.internal.StringUtil
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.schema.meta.MetaSchema
import zio.stream.*

import io.funkode.resource.model.*

type ResourceApiCall[R] = IO[ResourceError, R]

trait ResourceStore:

  def fetch(urn: Urn): ResourceApiCall[Resource]
  def store(resource: Resource): ResourceApiCall[Resource]

  def link(leftUrn: Urn, relType: String, rightUrn: Urn): ResourceApiCall[Unit]
  def fetchRel(urn: Urn, relType: String): ResourceStream[Resource]

object ResourceStore:

  type WithResourceStore[R] = ZIO[ResourceStore, ResourceError, R]
  type WithResourceStreamStore[R] = ZStream[ResourceStore, ResourceError, R]

  def withStore[R](f: ResourceStore => WithResourceStore[R]) = ZIO.service[ResourceStore].flatMap(f)

  def fetch(urn: Urn): WithResourceStore[Resource] = withStore(_.fetch(urn))

  def store(resource: Resource): WithResourceStore[Resource] =
    withStore(_.store(resource))

  def link(leftUrn: Urn, relType: String, rightUrn: Urn): WithResourceStore[Unit] =
    withStore(_.link(leftUrn, relType, rightUrn))

  def fetchRel(urn: Urn, relType: String): WithResourceStreamStore[Resource] =
    ZStream.service[ResourceStore].flatMap(_.fetchRel(urn, relType))

/*
trait JsonStore extends ResourceStore[JsonEncoder, JsonDecoder, Json]:
  override type DocResource = JsonResource

object JsonStore:

  type WithJsonStore[R] = ZIO[JsonStore, ResourceError, R]

  def withStore[R](f: JsonStore => WithJsonStore[R]) = ZIO.service[JsonStore].flatMap(f)

  def fetch(urn: Urn): WithJsonStore[JsonResource] = withStore(_.fetch(urn))

  def store[R: JsonEncoder](urn: Urn, r: R): WithJsonStore[JsonResource] =
    withStore(_.store(urn, r))

  def store[R: JsonEncoder: Identifiable](r: R): WithJsonStore[JsonResource] = store(r.urn, r)

  extension (resourceIO: WithJsonStore[JsonResource])
    def deserialize[R: JsonDecoder] = resourceIO.flatMap(_.deserialize[R])
 */
