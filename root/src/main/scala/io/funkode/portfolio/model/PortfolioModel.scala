/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.portfolio
package model

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
