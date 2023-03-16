/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package input

import io.lemonlabs.uri.Urn
import zio.*

import io.funkode.resource.model.*

type ResourceIO[R] = IO[ResourceError, R]

trait ResourceInputService:

  def getResourceByUrn(urn: Urn): ResourceIO[Resource]
  def upsertResource(resource: Resource): ResourceIO[Resource]
  // def deleteResource(urn: Urn): ResourceIO[Resource]

object ResourceInputService:

  type WithService[R] = ZIO[ResourceInputService, ResourceError, R]

  private def withService[R](f: ResourceInputService => WithService[R]): WithService[R] =
    ZIO.service[ResourceInputService].flatMap(f)

  def getResourceByUrn(urn: Urn): WithService[Resource] = withService(_.getResourceByUrn(urn))

  def upsertResource(resource: Resource): WithService[Resource] = withService(_.upsertResource(resource))

  // def deleteResource(urn: Urn): WithService[Resource] = withService(_.deleteResource(urn))
