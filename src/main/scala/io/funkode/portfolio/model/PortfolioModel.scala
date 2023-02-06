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

  given Resource.Identifiable[Network] with
    def nid: String = "network"
    def resourceNss(r: Network): String = r.id
    def resourceWithNss(n: Network)(newNss: String): Network = n.copy(id = newNss)

  given Resource.Identifiable[Transaction] with
    def nid: String = "tx"
    def resourceNss(transaction: Transaction): String =
      s"${transaction.hash}@${transaction.network.nss}"
    def resourceWithNss(transaction: Transaction)(newNss: String): Transaction =
      val Array(newHash, newNetworkNss) = newNss.split("@")
      val newNetwork = transaction.network.withNss(newNetworkNss)
      transaction.copy(network = newNetwork, hash = newHash)
