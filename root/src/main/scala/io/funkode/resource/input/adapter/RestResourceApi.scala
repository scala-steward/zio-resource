/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package input
package adapter

import io.lemonlabs.uri.Urn
import zio.*
import zio.http.*
import zio.http.Header.ContentType

import io.funkode.resource.model.*

object RestResourceApi:

  val app: Http[ResourceInputService, ResourceError, Request, Resource] = Http.collectZIO[Request]:
    case Method.GET -> Root / nid / nss => ResourceInputService.getResourceByUrn(Urn(nid, nss))

    case request @ Method.PUT -> Root / nid / nss =>
      if request.hasContentType("application/json") then
        ResourceInputService.upsertResource(Resource.fromJsonStream(Urn(nid, nss), request.body.asStream))
      else contentTypeNotSupportedError(request)

    // case Method.DELETE -> "" /: path => ResourceInputService.deleteResource(path)

  private def contentTypeNotSupportedError(request: Request) =
    ZIO.fail(
      ResourceError.FormatError(
        "Only application/json supported, found: " + request.headers.get(Header.ContentType.name)
      )
    )
