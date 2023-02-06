/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package model

import zio.*
import zio.schema.*
import zio.schema.Schema.Record
import zio.schema.meta.MetaSchema

import outbound.*
import outbound.adapter.ArangoResourceStore

case class ResourceModel(name: String, collections: Map[String, CollectionModel] = Map.empty)
case class CollectionModel(resourceType: String, rels: List[RelModel] = List.empty)
case class RelModel(rel: String, targetType: String, oneToMany: Boolean = false)
