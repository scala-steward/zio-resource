/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import io.funkode.resource.output.{ResourceApiCall, ResourceStore}
import io.lemonlabs.uri.Urn
import zio.json.JsonDecoder

case class Relationship[R](relType: String)

object Relationship:

  extension [R](rel: Relationship[R])
    def fetchFrom(
        urn: Urn
    )(using resourceStore: ResourceStore, D: JsonDecoder[R]): ResourceStream[Resource.Of[R]] =
      resourceStore.fetchRelAs[R](urn, rel.relType)

    def fetchFromAndConsume(
        urn: Urn
    )(using resourceStore: ResourceStore, D: JsonDecoder[R]): ResourceStream[Resource.InMemory[R]] =
      fetchFrom(urn).mapZIO(_.consume)

    def fetchOneFrom(urn: Urn)(using ResourceStore, JsonDecoder[R]): ResourceApiCall[Resource.Of[R]] =
      fetchFrom(urn).runHead.someOrFail(ResourceError.NotFoundError(urn, None))

    def fetchOneFromAndConsume(
        urn: Urn
    )(using ResourceStore, JsonDecoder[R]): ResourceApiCall[Resource.InMemory[R]] =
      fetchOneFrom(urn).flatMap(_.consume)

    def linkFromTo(
        urnFrom: Urn,
        urnTo: Urn
    )(using resourceStore: ResourceStore): ResourceApiCall[Unit] =
      resourceStore.link(urnFrom, rel.relType, urnTo)
