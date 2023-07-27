/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package model

import zio.test.*
import io.funkode.portfolio.model.*

trait PortfolioSampleModel:

  val mainPackage = "io.funkode.portfolio.model"

  val networkCollectionModel = CollectionModel(
    "network",
    s"$mainPackage.Network",
    List(RelModel("transactions", s"$mainPackage.Transaction", true))
  )
  val txCollectionModel = CollectionModel(
    "tx",
    s"$mainPackage.Transaction",
    List(RelModel("next", s"$mainPackage.Transaction", false))
  )

  def expectedModel(name: String) =
    ResourceModel(
      name,
      Map("network" -> networkCollectionModel, "tx" -> txCollectionModel)
    )

object StoreModelDerivationSpec extends ZIOSpecDefault with PortfolioSampleModel:

  override def spec: Spec[TestEnvironment, Any] =
    suite("Arango ResourceStore should")(
      test("Create ResourceModel from sealed trait") {

        val graphModel: ResourceModel = DeriveResourceModelTasty.gen[PortfolioTrait]
        assertTrue(
          graphModel == expectedModel("portfolioTrait")
        ) // && assertTrue(graphModelEnum == expectedModel)
      },
      test("Create ResourceModel from type") {

        val graphModel: ResourceModel = DeriveResourceModelTasty.gen[PortfolioType]
        assertTrue(graphModel == expectedModel("portfolioType"))
      },
      test("Create ResourceModel from case class") {

        val graphModel: ResourceModel = DeriveResourceModelTasty.gen[PortfolioCaseClass]
        assertTrue(graphModel == expectedModel("portfolioCaseClass"))
      },
      test("Create ResourceModel from And and Or types") {

        val graphModelAnd: ResourceModel = DeriveResourceModelTasty.gen[Network & Transaction]
        val graphModelOr: ResourceModel = DeriveResourceModelTasty.gen[Network | Transaction]

        assertTrue(graphModelAnd == expectedModel("networkAndTransaction")) &&
        assertTrue(graphModelOr == expectedModel("networkOrTransaction"))
      }
    )
