/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.portfolio

import zio.*

import io.funkode.portfolio.model.*
import io.funkode.resource.model.*
import io.funkode.resource.outbound.ResourceStore

type PortfolioCall[R] = IO[PortfolioError, R]

trait PortfolioService:

  import Portfolio.*

  def addNetwork(network: Network): PortfolioCall[Network]
  def addTransaction(transaction: Transaction): PortfolioCall[Transaction]
  def networkTransactions(network: Network): PortfolioCall[Iterator[Transaction]]
/*
object PortfolioService:

  class Impl[Enc[_], Dec[_], Body](resourceStore: ResourceStore[Enc, Dec, Body]) extends PortfolioService:

    def addNetwork(network: Network): PortfolioCall[Network] =
      resourceStore.store(network)

    def addTransaction(transaction: Transaction): PortfolioCall[Transaction] =
      resourceStore.store(transaction)

    def networkTransactions(network: Network): PortfolioCall[Iterator[Transaction]] =
      resourceStore.fetchRel(network, "transactions")
 */
