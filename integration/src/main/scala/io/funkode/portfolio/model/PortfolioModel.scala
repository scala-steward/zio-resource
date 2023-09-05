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

package io.funkode.portfolio.model

import io.lemonlabs.uri.Urn
import zio.json.*

import io.funkode.resource.model.*
import JsonUtils.given

sealed trait PortfolioTrait
type PortfolioType = Network & Transaction & Asset
case class PortfolioCaseClass(network: Network, transaction: Transaction, asset: Asset)

case class Network(
    id: String,
    chainId: String,
    name: String,
    currency: String,
    transactions: List[Transaction] = List.empty
) extends PortfolioTrait
    derives JsonCodec

case class Transaction(
    network: Urn,
    hash: String,
    timestamp: Long,
    next: Option[Transaction] = None
) extends PortfolioTrait
    derives JsonCodec

opaque type Symbol = String
object Symbol
def apply(symbol: String): Symbol = symbol
extension (symbol: Symbol) def unwrap: String = symbol

enum Asset extends PortfolioTrait:
  case Currency(symbol: Symbol, network: Network)
  case Token(symbol: Symbol, network: Network, smartContract: String)

object Network:

  given Resource.Addressable[Network] with
    def resourceNid: String = "network"
    def resourceNss(network: Network): String = network.id

object Transaction:

  given Resource.Addressable[Transaction] with
    def resourceNid: String = "tx"
    def resourceNss(transaction: Transaction): String =
      s"${transaction.hash}@${transaction.network.nss}"

object Asset:

  given Resource.Addressable[Asset] with
    def resourceNid: String = "asset"
    def resourceNss(asset: Asset): String = asset match
      case Asset.Currency(symbol, network) =>
        s"${symbol.unwrap}@${network.urn.nss}"
      case Asset.Token(symbol, network, smartContract) =>
        s"${symbol.unwrap}:${smartContract}@${network.urn.nss}"
