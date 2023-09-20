/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package output
package adapter

import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.*

import io.funkode.arangodb.http.*
import io.funkode.arangodb.http.JsonCodecs.given
import io.funkode.arangodb.model.*
import io.funkode.resource.model.*
import io.funkode.velocypack.VPack.*

class InTransaction(store: ArangoResourceStore, transactionId: TransactionId) extends ResourceStore:

  def resourceModel: ResourceModel = store.resourceModel

  def fetch(urn: Urn): ResourceStream[Resource] = store.fetchWithTx(urn)(Some(transactionId))

  def save(resource: Resource): ResourceApiCall[Resource] = store.saveWithTx(resource)(Some(transactionId))

  def delete(urn: Urn): ResourceApiCall[Resource] = store.deleteWithTx(urn)(Some(transactionId))

  def link(leftUrn: Urn, relType: String, rightUrn: Urn): ResourceApiCall[Unit] =
    store.linkWithTx(leftUrn, relType, rightUrn)(Some(transactionId))

  def fetchRel(urn: Urn, relType: String): ResourceStream[Resource] =
    store.fetchRelWithTx(urn, relType)(Some(transactionId))

  def transaction[R](body: ResourceStore => ResourceApiCall[R]): ResourceApiCall[R] =
    throw new UnsupportedOperationException("we can't nest transactions")

class ArangoResourceStore(db: ArangoDatabaseJson, storeModel: ResourceModel) extends ResourceStore:

  import ArangoResourceStore.*
  import ArangoResourceStore.given

  implicit val jsonCodec: JsonCodec[Json] = JsonCodec[Json](Json.encoder, Json.decoder)

  val graph = db.graph(GraphName(resourceModel.name))

  case class Rel(_rel: String, _from: DocumentHandle, _to: DocumentHandle, _key: DocumentKey)
      derives JsonCodec

  private def relCollection(urn: Urn): CollectionName = CollectionName(urn.nid + "-rels")

  private def generateLinkKey(leftUrn: Urn, rel: String, rightUrn: Urn) =
    DocumentKey(s"""${leftUrn.nss}-(${rel})-${rightUrn.nss}""")

  def resourceModel: ResourceModel = storeModel

  private def pureJsonPipeline(urn: Urn) = ZPipeline.fromFunction[Any, ResourceError, Byte, Resource]:
    byteStream =>
      if byteStream == null then ZStream.fail(ResourceError.NotFoundError(urn))
      else
        val jsonFromStream = JsonDecoder[Json].decodeJsonStreamInput(byteStream).handleErrors(urn)
        ZStream.fromZIO(jsonFromStream.map(_.asResource))

  def fetchWithTx(urn: Urn)(transaction: Option[TransactionId]): ResourceStream[Resource] =
    db
      .document(urn)
      .readRaw(transaction = transaction)
      .handleStreamErrors(urn)
      .via(pureJsonPipeline(urn))
      .orElseIfEmpty(ZStream.fail(ResourceError.NotFoundError(urn, None)))

  def fetch(urn: Urn): ResourceStream[Resource] =
    fetchWithTx(urn)(None)

  def saveWithTx(resource: Resource)(transaction: Option[TransactionId]): ResourceApiCall[Resource] =
    resource.format match
      case ResourceFormat.Json =>
        val urn = resource.urn

        for
          jsonDocument <- JsonDecoder[Json]
            .decodeJsonStreamInput(resource.body)
            .catchAll:
              case t: Throwable =>
                ZIO.fail(ResourceError.SerializationError("Error reading json resource body", Some(t)))
          vobject <- jsonDocument match
            case jsonObj: Json.Obj =>
              ZIO.succeed(JsonCodecs.jsonObjectToVObject(jsonObj))
            case other =>
              ZIO.fail(ResourceError.FormatError(s"only supported to store json objects, received $other"))
          savedResource <- db
            .document(urn)
            .upsert(vobject, transaction)
            .handleErrors(urn)
            .flatMap(vpack =>
              ZIO
                .fromEither[String, Json](vobjectEncoder.toJsonAST(vpack))
                .catchAll(encodeError => ZIO.fail(ResourceError.SerializationError(encodeError)))
            )
            .map(_.asResource)
        yield savedResource

  def save(resource: Resource): ResourceApiCall[Resource] =
    saveWithTx(resource)(None)

  def deleteWithTx(urn: Urn)(transaction: Option[TransactionId]): ResourceApiCall[Resource] =
    for
      deleted <- fetchWithTx(urn)(transaction).runHead.someOrFail(ResourceError.NotFoundError(urn, None))
      _ <- graph.vertexDocument(urn).remove[Json](transaction = transaction).handleErrors(urn)
    yield deleted

  def delete(urn: Urn): ResourceApiCall[Resource] =
    deleteWithTx(urn)(None)

  def linkWithTx(leftUrn: Urn, rel: String, rightUrn: Urn)(
      transaction: Option[TransactionId]
  ): ResourceApiCall[Unit] =
    val linkKey = generateLinkKey(leftUrn, rel, rightUrn)
    db.collection(relCollection(leftUrn))
      .documents
      .insert(document = Rel(rel, leftUrn, rightUrn, linkKey), transaction = transaction)
      .ignoreConflict
      .handleErrors(Urn.apply("rels", linkKey.unwrap))
      .map(_ => ())

  def link(leftUrn: Urn, rel: String, rightUrn: Urn): ResourceApiCall[Unit] =
    linkWithTx(leftUrn, rel, rightUrn)(None)

  def fetchRelWithTx(urn: Urn, relType: String)(
      transaction: Option[TransactionId]
  ): Stream[ResourceError, Resource] =
    val query = db
      .query(
        Query("FOR v, e IN OUTBOUND @startVertex @@edge FILTER e._rel == @relType RETURN v")
          .bindVar("startVertex", VString(fromUrnToDocHandle(urn).unwrap))
          .bindVar("@edge", VString(relCollection(urn).unwrap))
          .bindVar("relType", VString(relType))
      )
    val queryWithTx = transaction.map(t => query.transaction(t)).getOrElse(query)

    queryWithTx
      .stream[Json]
      .map(json => json.asResource)
      .handleStreamErrors(urn)

  def fetchRel(urn: Urn, relType: String): Stream[ResourceError, Resource] =
    fetchRelWithTx(urn, relType)(None)

  def transaction[R](
      body: ResourceStore => io.funkode.resource.output.ResourceApiCall[R]
  ): ResourceApiCall[R] =
    for
      tx <- db.transactions
        .begin(write = resourceModel.allCollectionNames, waitForSync = true)
        .handleErrors(Urn("not", "applyTx"))
      storeWithTx = InTransaction(this, tx.id)
      result <- body(storeWithTx).catchAll(e =>
        tx.abort.handleErrors((Urn("not", "applyAbort"))) *> ZIO.fail(e)
      )
      _ <- tx.commit.handleErrors(Urn("not", "applyCommit"))
    yield result

object ArangoResourceStore:

  import ArangoError.*
  import ResourceError.*

  val RelsCollection = CollectionName("rels")

  val InternalKeys = Seq(VObject.IdKey, VObject.KeyKey, VObject.RevKey)

  given fromUrnToDocHandle: Conversion[Urn, DocumentHandle] = urn =>
    DocumentHandle(CollectionName(urn.nid), DocumentKey(urn.nss))

  given fromDocHandleToUrn: Conversion[DocumentHandle, Urn] = docHandle =>
    Urn.parse(s"urn:${docHandle.collection.unwrap}:${docHandle.key.unwrap}")

  def handleArangoErrors(urn: Urn, t: Throwable): ResourceError = t match
    case notFoundError @ ArangoError(404, _, _, _) => ResourceError.NotFoundError(urn, Some(notFoundError))
    case resourceError: ResourceError              => resourceError
    case otherError =>
      if otherError != null then
        otherError.getCause match
          case causeNotFound @ ArangoError(404, _, _, _) =>
            ResourceError.NotFoundError(urn, Some(causeNotFound))
          case causeResourceError: ResourceError => causeResourceError
          case other =>
            if other != null then
              other.getCause match
                case resourceError: ResourceError =>
                  resourceError
                case _ =>
                  ResourceError.UnderlinedError(otherError)
            else ResourceError.UnderlinedError(otherError)
      else ResourceError.UnderlinedError(otherError)

  extension [R](io: IO[Throwable, R])
    def handleErrors(urn: Urn): ResourceApiCall[R] =
      io.mapError(handleArangoErrors(urn, _))

  extension [R](stream: Stream[Throwable, R])
    def handleStreamErrors(urn: Urn): ResourceStream[R] =
      stream.mapError(handleArangoErrors(urn, _))

  extension (json: Json)
    def etag: Option[Etag] = json match
      case Json.Obj(fields) =>
        fields.filter(_._1 == VObject.RevKey).map(_._2.as[String].toOption.map(Etag.apply)).headOption.flatten
      case _ =>
        None

    def documentHandle: DocumentHandle = json match
      case Json.Obj(fields) =>
        fields
          .filter(_._1 == VObject.IdKey)
          .map(_._2.as[String].toOption.flatMap(DocumentHandle.parse))
          .headOption
          .flatten
          .get // risky option but ArangoDB always retrieves _id
      case other => throw new Exception(s"not expected an ArangoDB document without _id, received: $other")

    def pure: Json = json match
      case Json.Obj(fields) => Json.Obj(fields.filterNot(t => InternalKeys.contains(t._1)))
      case other            => other

    def asResource: Resource =
      val urn: Urn = fromDocHandleToUrn(json.documentHandle)
      val body: ByteResourceStream = ZStream.fromIterable(json.pure.toJson.toCharArray.map(_.toByte))
      val etag: Option[Etag] = json.etag

      Resource.fromJsonStream(urn, body, etag)

  extension [R, O](zio: ZIO[R, ArangoError, O])
    def ignoreConflict: ZIO[R, ArangoError, Unit] = zio
      .map(_ => ())
      .ifConflict(ZIO.succeed(()))

  extension (collectionName: CollectionName)
    def toRelName: CollectionName =
      val unwrapped = collectionName.unwrap
      if unwrapped.contains("-rels") then collectionName else CollectionName(unwrapped + "-rels")
    def graphEdgeDefinition(collections: List[CollectionName]): GraphEdgeDefinition =
      GraphEdgeDefinition(collectionName.toRelName, List(collectionName), collections)

  extension (resourceModel: ResourceModel)
    def collectionNames = resourceModel.collections
      .map(_._1)
      .map(CollectionName.apply)
      .toList
    def relCollectionNames = resourceModel.collectionNames.map(_.toRelName)
    def allCollectionNames = resourceModel.collectionNames ++ resourceModel.relCollectionNames

  def initDb(arango: ArangoClientJson, resourceModel: ResourceModel): ResourceApiCall[ArangoDatabaseJson] =
    val db = arango.database(DatabaseName(resourceModel.name))
    val graph = db.graph(GraphName(resourceModel.name))
    for
      _ <- ZIO.logInfo(s"Creating database (if not exist) ${db.name}")
      _ <- db.createIfNotExist().handleErrors(Urn("init", "db"))
      _ <- ZIO.logInfo(s"Creating graph (if not exist) ${graph.name}")
      _ <- graph.create().ignoreConflict.handleErrors(Urn("init", "db"))
      existingVertices <- graph.vertexCollections.handleErrors(Urn("init", "db"))
      _ <- ZIO.logInfo(s"""Existing vertices ${existingVertices.mkString(", ")}""")
      existingEdges <- graph.edgeCollections.handleErrors(Urn("init", "db"))
      _ <- ZIO.logInfo(s"""Existing edges ${existingEdges.mkString(", ")}""")
      _ <- ZIO
        .collectAll:
          val collectionNames = resourceModel.collectionNames
          val collectionEdgeNames = collectionNames.map(_.toRelName)
          val pendingCollections = collectionNames.diff(existingVertices)
          val toBeUpdatedEdges = collectionEdgeNames.intersect(existingEdges)
          val newEdges = collectionEdgeNames.diff(existingEdges)

          val createCollections =
            pendingCollections.map(col => graph.addVertexCollection(col).ignoreConflict)

          val createRels = newEdges
            .map(_.graphEdgeDefinition(collectionNames))
            .map(edge => graph.addEdgeCollection(edge.collection, edge.from, edge.to).ignoreConflict)

          val updateRels = toBeUpdatedEdges
            .map(_.graphEdgeDefinition(collectionNames))
            .map(edge => graph.replaceEdgeCollection(edge.collection, edge.from, edge.to).ignoreConflict)

          ZIO.logInfo(s"""Collections to be created:
               |collectionNames: ${collectionNames.map(_.unwrap).mkString(", ")}
               |existingEdges: ${existingEdges.map(_.unwrap).mkString(", ")}
               |pendingCollections = ${pendingCollections.map(_.unwrap).mkString(", ")}
               |newEdges = ${newEdges.map(_.unwrap).mkString(", ")}
               |toBeUpdatedEdges = ${toBeUpdatedEdges.map(_.unwrap).mkString(", ")}
               |""".stripMargin) +: (createCollections ++ createRels ++ updateRels)
        .handleErrors(Urn("init", "db"))
    yield db

  inline def derived[R]: ZLayer[ArangoClientJson, ResourceError, ResourceStore] =
    ZLayer(
      for
        client <- ZIO.service[ArangoClientJson]
        resourceModel = DeriveResourceModelTasty.gen[R]
        db <- initDb(client, resourceModel)
      yield new ArangoResourceStore(db, resourceModel)
    )
