package io.funkode.resource
package output

import io.lemonlabs.uri.Urn
import zio.*
import zio.http.*
import zio.json.*
import zio.stream.*
import zio.test.*

import io.funkode.arangodb.*
import io.funkode.arangodb.http.*
import io.funkode.portfolio
import io.funkode.portfolio.model.*
import io.funkode.resource.model.*
import io.funkode.resource.model.Resource.*
import adapter.ArangoResourceStore

trait TransactionsExamples:

  case class BadModel(asdfasdfadsfsa: String) derives JsonCodec

  given Addressable[BadModel] with
    def resourceNid: String = "badModel"
    def resourceNss(r: BadModel): String = r.asdfasdfadsfsa

  val hash1 = "0x888333"
  val hash2 = "0x999333"
  val timestamp1 = 1L
  val timestamp2 = 2L

  val ethNetworkUrn = Urn.parse("urn:network:eth")
  val tx1Urn = Urn.parse("urn:tx:" + hash1 + "@" + ethNetworkUrn.nss)
  val tx2Urn = Urn.parse("urn:tx:" + hash2 + "@" + ethNetworkUrn.nss)

  val ethNetwork = Network("eth", "1", "Ethereum Mainnet", "ETH")
  val bscNetwork = Network("bsc", "1", "B Chain Mainnet", "BNB")
  val ethNetwornJsonString =
    """
      |{
      |  "id": "eth",
      |  "chainId": "1",
      |  "name": "Ethereum Mainnet",
      |  "currency": "ETH"
      |}
      |""".stripMargin

  val ethNetworkResource =
    Resource.fromJsonStream(ethNetworkUrn, ZStream.fromIterable(ethNetwornJsonString.getBytes))

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

  val tx1 = Transaction(ethNetwork.urn, hash1, timestamp1)
  val tx2 = Transaction(ethNetwork.urn, hash2, timestamp2)
  val tx1Resource = Resource.fromJsonStream(tx1Urn, ZStream.fromIterable(tx1JsonString.getBytes()))
  val tx2Resource = Resource.fromJsonStream(tx2Urn, ZStream.fromIterable(tx2JsonString.getBytes()))

object ArangoStoreSpec extends ZIOSpecDefault with TransactionsExamples:

  override def spec: Spec[TestEnvironment, Any] =
    suite("Arango ResourceStore should")(
      test("Store network, transaction and link them") {

        for
          storedNetworkResource <- ResourceStore.save(ethNetwork)
          storedNetwork <- storedNetworkResource.body
          fetchedNetworkResource <- ResourceStore.fetchOneAs[Network](ethNetworkUrn)
          fetchedNetwork <- fetchedNetworkResource.body
          storedTxResource <- ResourceStore.save(tx1)
          storedTx <- storedTxResource.body
          fetchedTxResource <- ResourceStore.fetchOneAs[Transaction](tx1Urn)
          fetchedTx <- fetchedTxResource.body
          // link test
          _ <- ResourceStore.save(tx2)
          _ <- ResourceStore.link(ethNetwork.urn, "transactions", tx1.urn)
          _ <- ResourceStore.link(ethNetwork.urn, "transactions", tx2.urn)
          _ <- ResourceStore.link(tx1.urn, "network", ethNetwork.urn)
          _ <- ResourceStore.link(tx2.urn, "network", ethNetwork.urn)
          networkTransactions <- ResourceStore
            .fetchRelAs[Transaction](ethNetwork.urn, "transactions")
            .mapZIO(_.body)
            .runCollect
          transactionNetworkResource <- ResourceStore.fetchOneRelAs[Network](tx1.urn, "network")
          transactionNetwork <- transactionNetworkResource.body
          _ <- ResourceStore.delete(tx1.urn)
          netWorkTransactionsAfterDelete <- ResourceStore
            .fetchRelAs[Transaction](ethNetwork.urn, "transactions")
            .mapZIO(_.body)
            .runCollect
        yield assertTrue(storedNetwork == ethNetwork) &&
          assertTrue(storedNetwork == fetchedNetwork) &&
          assertTrue(fetchedNetworkResource.etag.nonEmpty) &&
          assertTrue(storedTx == tx1) &&
          assertTrue(storedTx == fetchedTx) &&
          assertTrue(transactionNetworkResource.urn == ethNetworkUrn) &&
          assertTrue(transactionNetwork == ethNetwork) &&
          assertTrue(networkTransactions.sortBy(_.timestamp) == Chunk(tx1, tx2)) &&
          assertTrue(netWorkTransactionsAfterDelete == Chunk(tx2))
      },
      test("Manage not found error in raw resource") {
        val fakeUrn = Urn.parse("urn:network:doesnt:exist")
        for error <- ResourceStore
            .fetch(fakeUrn)
            .runHead
            .flip
        yield assertTrue(error match
          case ResourceError.NotFoundError(urn, _) => urn == fakeUrn
          case _                                   => false
        )
      },
      test("Manage not found error in typed resource") {
        val fakeUrn = Urn.parse("urn:network:doesnt:exist")
        for error <- ResourceStore
            .fetchAs[Network](fakeUrn)
            .runCollect
            .flip
        yield assertTrue(error match
          case ResourceError.NotFoundError(urn, _) => urn == fakeUrn
          case _                                   => false
        )
      },
      test("Manage not found error in (fetchOne) raw resource") {
        val fakeUrn = Urn.parse("urn:network:doesnt:exist")
        for error <- ResourceStore
            .fetchOne(fakeUrn)
            .flip
        yield assertTrue(error match
          case ResourceError.NotFoundError(urn, _) => urn == fakeUrn
          case _                                   => false
        )
      },
      test("Manage not found error in (fetchOne) typed resource") {
        val fakeUrn = Urn.parse("urn:network:doesnt:exist")
        for error <- ResourceStore
            .fetchOneAs[Network](fakeUrn)
            .flip
        yield assertTrue(error match
          case ResourceError.NotFoundError(urn, _) => urn == fakeUrn
          case _                                   => false
        )
      },
      test("Manage serialization errors in typed resource") {
        for
          _ <- ResourceStore.save[Network](bscNetwork)
          error <- ResourceStore
            .fetchAs[BadModel](bscNetwork.urn)
            .mapZIO(_.body)
            .runCollect
            .flip
        yield assertTrue(error match
          case _: ResourceError.SerializationError => true
          case _                                   => false
        )
      }
    ).provideShared(
      ArangoConfiguration.default,
      Client.default,
      ArangoClientJson.testContainers,
      ArangoResourceStore.derived[PortfolioTrait]
    )
