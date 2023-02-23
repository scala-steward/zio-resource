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

sealed trait Portfolio {}

case class Network(
    id: String,
    chainId: String,
    name: String,
    currency: String,
    transactions: List[Transaction] = List.empty
) extends Portfolio
    derives JsonCodec

case class Transaction(
    network: Network,
    hash: String,
    timestamp: Long,
    next: Option[Transaction] = None
) extends Portfolio
    derives JsonCodec

object Portfolio:

  given Resource.Typed[Network] with
    def resourceCollection: String = "network"
    def resourceId(r: Network): String = r.id
    def resourceWithId(n: Network)(newId: String): Network = n.copy(id = newId)

  given Resource.Typed[Transaction] with
    def resourceCollection: String = "tx"
    def resourceId(transaction: Transaction): String =
      s"${transaction.hash}@${transaction.network.id}"
    def resourceWithId(transaction: Transaction)(newId: String): Transaction =
      val Array(newHash, newNetworkId) = newId.split("@")
      val newNetwork = transaction.network.withId(newNetworkId)
      transaction.copy(network = newNetwork, hash = newHash)
