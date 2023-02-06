package io.funkode.resource
package outbound

import io.funkode.arangodb.*
import io.funkode.arangodb.http.*
import io.funkode.arangodb.model.*
import io.funkode.arangodb.http.JsonCodecs.given
import io.lemonlabs.uri.{Url, Urn}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.http.*
import zio.test.*
import adapter.ArangoResourceStore
import io.funkode.portfolio
import io.funkode.portfolio.model.*
import io.funkode.resource.model.*
import io.funkode.resource.model.given
import io.funkode.resource.model.Resource.*

trait TransactionsExamples:

  import Portfolio.given

  val hash1 = "0x888333"
  val timestamp1 = 1L

  val ethNetworkUrn = Urn.parse("urn:network:eth")
  val tx1Urn = Urn.parse("urn:tx:" + hash1 + "@" + ethNetworkUrn.nss)

  val ethNetwork = Network("eth", "1", "Ethereum Mainnet", "ETH")

  val tx1 = io.funkode.portfolio.model.Transaction(ethNetwork, hash1, timestamp1)

object ArangoStoreIT extends ZIOSpecDefault with TransactionsExamples:

  override def spec: Spec[TestEnvironment, Any] =
    suite("Arango ResourceStore should")(test("Store transaction") {
      for
        storedNetwork <- JsonStore.store(ethNetwork).deserialize[Network]
        fetchedNetwork <- JsonStore.fetch(ethNetworkUrn).deserialize[Network]
        storedTx <- JsonStore.store(tx1).deserialize[portfolio.model.Transaction]
        fetchedTx <- JsonStore.fetch(tx1Urn).deserialize[portfolio.model.Transaction]
      yield assertTrue(storedTx == tx1) &&
        assertTrue(fetchedTx == tx1) &&
        assertTrue(storedNetwork == ethNetwork) &&
        assertTrue(fetchedNetwork == ethNetwork)
    }).provideShared(
      Scope.default,
      ArangoConfiguration.default,
      Client.default,
      ArangoClientJson.testContainers,
      ArangoResourceStore.derived[Portfolio]
    )
