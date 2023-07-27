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
type PortfolioType = Network & Transaction
case class PortfolioCaseClass(network: Network, transaction: Transaction)

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

object Network:

  given Resource.Addressable[Network] with
    def resourceNid: String = "network"
    def resourceNss(network: Network): String = network.id

object Transaction:

  given Resource.Addressable[Transaction] with
    def resourceNid: String = "tx"
    def resourceNss(transaction: Transaction): String =
      s"${transaction.hash}@${transaction.network.nss}"
