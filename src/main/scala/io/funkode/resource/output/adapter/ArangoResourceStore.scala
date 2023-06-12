/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package output
package adapter

import scala.deriving.Mirror

import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.*

import io.funkode.arangodb.http.*
import io.funkode.arangodb.http.JsonCodecs.given
import io.funkode.arangodb.model.*
import io.funkode.arangodb.protocol.ArangoMessage.*
import io.funkode.resource.model.*
import io.funkode.velocypack.VPack.*

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

  def fetch(urn: Urn): ResourceStream[Resource] =
    for
      headers <- ZStream.fromZIO(db.document(urn).head().handleErrors(urn))
      bodyStream = db
        .document(urn)
        .readRaw()
        .handleStreamErrors(urn)
      resourceEtag = headers match
        case Header.Response(_, _, _, meta) =>
          meta.get("Etag").map(Etag.apply)
        case _ => None
      resource <- ZStream.apply(Resource.fromJsonStream(urn, bodyStream, resourceEtag))
    yield resource

  def save(resource: Resource): ResourceApiCall[Resource] =
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
            .upsert(vobject)
            .handleErrors(urn)
            .flatMap(vpack =>
              ZIO
                .fromEither[String, Json](vobjectEncoder.toJsonAST(vpack))
                .catchAll(encodeError => ZIO.fail(ResourceError.SerializationError(encodeError)))
            )
            .map(_.asResource)
        yield savedResource

  def delete(urn: Urn): ResourceApiCall[Unit] =
    for
      _ <- fetchOne(urn)
      _ <- graph.vertexDocument(urn).remove[Json]().handleErrors(urn)
    yield ()

  def link(leftUrn: Urn, rel: String, rightUrn: Urn): ResourceApiCall[Unit] =
    val linkKey = generateLinkKey(leftUrn, rel, rightUrn)
    db.collection(relCollection(leftUrn))
      .documents
      .insert(Rel(rel, leftUrn, rightUrn, linkKey))
      .ignoreConflict
      .handleErrors(Urn.apply("rels", linkKey.unwrap))
      .map(_ => ())

  def fetchRel(urn: Urn, relType: String): Stream[ResourceError, Resource] =
    db
      .query(
        Query("FOR v, e IN OUTBOUND @startVertex @@edge FILTER e._rel == @relType RETURN v")
          .bindVar("startVertex", VString(fromUrnToDocHandle(urn).unwrap))
          .bindVar("@edge", VString(relCollection(urn).unwrap))
          .bindVar("relType", VString(relType))
      )
      .stream[Json]
      .map(json => json.asResource)
      .handleStreamErrors(urn)

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
    case e @ ArangoError(404, _, _, _) => ResourceError.NotFoundError(urn, Some(e))
    case e                             => ResourceError.UnderlinedError(e)

  extension [R](io: IO[Throwable, R])
    def handleErrors(urn: Urn): ResourceApiCall[R] =
      io.mapError(e => handleArangoErrors(urn, e))

  extension [R](stream: Stream[Throwable, R])
    def handleStreamErrors(urn: Urn): ResourceStream[R] =
      stream.catchAll(t =>
        ZIO.logErrorCause(s"handleStreamErrors for urn $urn", Cause.fail(t))
        ZStream.fail(handleArangoErrors(urn, t))
      )

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
          .map(_._2.as[String].toOption.map(DocumentHandle.parse).flatten)
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

  def initDb(arango: ArangoClientJson, resourceModel: ResourceModel): ResourceApiCall[ArangoDatabaseJson] =
    val db = arango.database(DatabaseName(resourceModel.name))
    val graph = db.graph(GraphName(resourceModel.name))
    for
      _ <- db.createIfNotExist().handleErrors(Urn("init", "db"))
      _ <- graph.create().ignoreConflict.handleErrors(Urn("init", "db"))
      existingVertices <- graph.vertexCollections.handleErrors(Urn("init", "db"))
      existingEdges <- graph.edgeCollections.handleErrors(Urn("init", "db"))
      _ <- ZIO
        .collectAll:
          val collections = resourceModel.collections
            .map(_._1)
            .map(CollectionName.apply)
            .toList
            .diff(existingVertices)

          val createCollections =
            collections.map(col => graph.addVertexCollection(col).ignoreConflict)

          val createRels = collections
            .map { sourceCollection =>
              val edgeName = CollectionName(sourceCollection.unwrap + "-rels")
              GraphEdgeDefinition(edgeName, List(sourceCollection), collections)
            }
            .map(edge => graph.addEdgeCollection(edge.collection, edge.from, edge.to).ignoreConflict)

          createCollections ++ createRels
        .handleErrors(Urn("init", "db"))
    yield db

  inline def derived[R: Mirror.Of]: ZLayer[ArangoClientJson, ResourceError, ResourceStore] =
    ZLayer(
      for
        client <- ZIO.service[ArangoClientJson]
        resourceModel = DeriveResourceModel.gen[R]
        db <- initDb(client, resourceModel)
      yield new ArangoResourceStore(db, resourceModel)
    )
