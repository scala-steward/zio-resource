/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package model

import scala.compiletime.*

import zio.json.*
import zio.schema.*
import zio.test.*

trait PortfolioSampleModel:

  val expectedModel =
    ResourceModel(
      "portfolio",
      Map(
        "networks" -> CollectionModel(
          "io.funkode.resource.model.Network",
          List(RelModel("transactions", "Sequence(io.funkode.resource.model.Transaction)"))
        ),
        "tx" -> CollectionModel(
          "io.funkode.resource.model.Transaction",
          List(RelModel("network", "io.funkode.resource.model.Network"))
        )
      )
    )

object StoreModelDerivationSpec extends ZIOSpecDefault with PortfolioSampleModel:

  override def spec: Spec[TestEnvironment, Any] =
    suite("Arango ResourceStore should")(test("Create graph from model") {

      // given portfolioSchema: Schema[Portfolio] = DeriveSchema.gen[Portfolio]
      val graphModel: ResourceModel = ResourceModelDerivation.gen[Portfolio]
      // val graphModelEnum: ResourceModel = ResourceModelDerivation.gen[PortfolioEnum]
      assertTrue(graphModel == expectedModel) // && assertTrue(graphModelEnum == expectedModel)
    })
