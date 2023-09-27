/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import io.lemonlabs.uri.Urn

enum ResourceError(msg: String, cause: Option[Throwable] = None) extends Throwable(msg, cause.orNull):
  case NotFoundError(urn: Urn, cause: Option[Throwable] = None)
      extends ResourceError(
        s"NotFoundError(urn: $urn, cause:  ${cause.map(_.getMessage).getOrElse("[empty cause]")}",
        cause
      )
  case SerializationError(msg: String, cause: Option[Throwable] = None) extends ResourceError(msg, cause)
  case FormatError(msg: String, cause: Option[Throwable] = None)
      extends ResourceError(s"Format not supported: $msg", cause)
  case UnderlinedError(cause: Throwable)
      extends ResourceError(s"Non controlled error, cause: ${cause.getMessage}", Some(cause))
