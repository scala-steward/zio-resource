/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.portfolio

enum PortfolioError(msg: String, cause: Option[Throwable]) extends Throwable(msg, cause.getOrElse(null)):
  case Underlying(cause: Throwable) extends PortfolioError("Not identified error", Some(cause))
