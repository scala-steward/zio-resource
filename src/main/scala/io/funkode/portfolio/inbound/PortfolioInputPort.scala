/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.portfolio
package inbound

import zio.*

type InputPortCall[R] = ZIO[PortfolioService, PortfolioError, R]
/*
trait PortfolioInputPort[Encoder[_], Decoder[_], Body]:

  private def withService[R](body: PortfolioService => PortfolioCall[R]): InputPortCall[R] =
    ZIO.service[PortfolioService].flatMap(body)

  def createNetwork(
      networkResource: Resource[Encoder, Decoder, Body]
  ): InputPortCall[Resource[Encoder, Decoder, Body]] =
    for
      network <- networkResource.deserialize[Network]
      createdNetwork <- withService(_.addNetwork(network))
    yield createdNetwork

  def createTransaction(
      networkResource: Resource[Encoder, Decoder, Body]
  ): InputPortCall[Resource[Encoder, Decoder, Body]] =
    for
      transaction <- networkResource.deserialize[Transaction]
      createdNetwork <- withService(_.addTransaction(transaction))
    yield createdNetwork
 */
