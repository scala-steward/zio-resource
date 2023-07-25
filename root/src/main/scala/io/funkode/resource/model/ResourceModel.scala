/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package model

import io.lemonlabs.uri.Urn

case class ResourceModel(name: String, collections: Map[String, CollectionModel] = Map.empty)
case class CollectionModel(collectionName: String, resourceType: String, rels: List[RelModel] = List.empty)
case class RelModel(rel: String, targetType: String, oneToMany: Boolean = false)

object ResourceModel:

  extension (model: ResourceModel)
    inline def collectionForUrn(urn: Urn): Option[CollectionModel] = model.collections.get(urn.nid)
