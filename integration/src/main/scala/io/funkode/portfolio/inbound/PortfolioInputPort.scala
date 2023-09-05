/*
 * Copyright (c) 2023 io.funkode
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
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
