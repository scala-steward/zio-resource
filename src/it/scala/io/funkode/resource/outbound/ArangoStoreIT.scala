package io.funkode.resource
package outbound

import io.funkode.arangodb.*
import io.funkode.arangodb.http.*
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
import zio.stream.*

trait TransactionsExamples:

  import Portfolio.given

  val hash1 = "0x888333"
  val hash2 = "0x999333"
  val timestamp1 = 1L
  val timestamp2 = 2L

  val ethNetworkUrn = Urn.parse("urn:network:eth")
  val tx1Urn = Urn.parse("urn:tx:" + hash1 + "@" + ethNetworkUrn.nss)
  val tx2Urn = Urn.parse("urn:tx:" + hash2 + "@" + ethNetworkUrn.nss)

  val ethNetwork = Network("eth", "1", "Ethereum Mainnet", "ETH")
  val ethNetwornJsonString =
    """
      |{
      |  "id": "eth",
      |  "chainId": "1",
      |  "name": "Ethereum Mainnet",
      |  "currency": "ETH"
      |}
      |""".stripMargin

  val ethNetworkResource = Resource.apply(ethNetworkUrn, ZStream.fromIterable(ethNetwornJsonString.getBytes))

  val tx1JsonString =
    s"""
       |{
       |  "network": $ethNetwornJsonString,
       |  "hash": "0x888333",
       |  "timestamp": 1
       |}
       |""".stripMargin

  val tx2JsonString =
    s"""
       |{
       |  "network": $ethNetwornJsonString,
       |  "hash": "0x999333",
       |  "timestamp": 2
       |}
       |""".stripMargin

  val tx1 = Transaction(ethNetwork, hash1, timestamp1)
  val tx2 = Transaction(ethNetwork, hash2, timestamp2)
  val tx1Resource = Resource.apply(tx1Urn, ZStream.fromIterable(tx1JsonString.getBytes()))
  val tx2Resource = Resource.apply(tx2Urn, ZStream.fromIterable(tx2JsonString.getBytes()))

object ArangoStoreIT extends ZIOSpecDefault with TransactionsExamples:

  override def spec: Spec[TestEnvironment, Any] =
    suite("Arango ResourceStore should")(test("Store network, transaction and link them") {
      for
        storedNetworkResource <- ResourceStore.store(ethNetworkResource)
        storedNetwork <- storedNetworkResource.of[Network].body
        fetchedNetworkResource <- ResourceStore.fetch(ethNetworkUrn)
        fetchedNetwork <- fetchedNetworkResource.of[Network].body
        storedTxResource <- ResourceStore.store(tx1Resource)
        storedTx <- storedTxResource.of[Transaction].body
        fetchedTxResource <- ResourceStore.fetch(tx1Urn)
        fetchedTx <- fetchedTxResource.of[Transaction].body
        // link test
        _ <- ResourceStore.store(tx2Resource)
        _ <- ResourceStore.link(ethNetworkUrn, "transactions", tx1Urn)
        _ <- ResourceStore.link(ethNetworkUrn, "transactions", tx2Urn)
        _ <- ResourceStore.link(tx1Urn, "network", ethNetworkUrn)
        networkTransactions <- ResourceStore
          .fetchRel(ethNetworkUrn, "transactions")
          .mapZIO(_.of[Transaction].body)
          .run(ZSink.collectAll)
        transactionNetworkResource <- ResourceStore.fetchRel(tx1Urn, "network").run(ZSink.head)
        transactionNetwork <- transactionNetworkResource
          .map(_.of[Network].body)
          .getOrElse(throw new Exception())
        _ <- ZIO.unit
      yield assertTrue(storedNetwork == ethNetwork) &&
        assertTrue(storedNetwork == fetchedNetwork) &&
        assertTrue(storedTx == tx1) &&
        assertTrue(storedTx == fetchedTx) &&
        assertTrue(transactionNetworkResource.map(_.id) == Some(ethNetworkUrn)) &&
        assertTrue(transactionNetwork == ethNetwork) &&
        assertTrue(networkTransactions.sortBy(_.timestamp) == Chunk(tx1, tx2))

    }).provideShared(
      Scope.default,
      ArangoConfiguration.default,
      Client.default,
      ArangoClientJson.testContainers,
      ArangoResourceStore.derived[Portfolio]
    )
