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
  val txCollectionModel =
    CollectionModel("tx", s"$mainPackage.Transaction", List())

  val expectedModel =
    ResourceModel(
      "portfolio",
      Map("network" -> networkCollectionModel, "tx" -> txCollectionModel)
    )

object StoreModelDerivationSpec extends ZIOSpecDefault with PortfolioSampleModel:

  override def spec: Spec[TestEnvironment, Any] =
    suite("Arango ResourceStore should")(test("Create graph from model") {

      // inline given portfolioSchema: Schema[Portfolio] = DeriveSchema.gen[Portfolio]
      val graphModel: ResourceModel = DeriveResourceModel.gen[Portfolio]

      // given portfolioSchemaEnum: Schema[PortfolioEnum] = DeriveSchema.gen[PortfolioEnum]
      // val graphModelEnum: ResourceModel = ResourceModelDerivation.gen[PortfolioEnum]

      assertTrue(graphModel == expectedModel) // && assertTrue(graphModelEnum == expectedModel)
    })
