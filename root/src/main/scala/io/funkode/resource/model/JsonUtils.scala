/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.annotation.{switch, tailrec}
import scala.collection.mutable.Stack

import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
import zio.json.JsonDecoder.JsonError
import zio.json.ast.Json
import zio.json.internal.*
import zio.stream.*

object JsonUtils:

  given urnCodec: JsonCodec[Urn] = JsonCodec(
    JsonEncoder[String].contramap(_.toString),
    JsonDecoder[String].map(Urn.parse)
  )

  enum JsonParsingPhase:
    case Root
    case Array
    case Object

  def normalizeJsonStream(jsonStream: ByteResourceStream): ResourceStream[Json] =
    for
      in <- ZStream.scoped:
        jsonStream.toInputStream.flatMap: is =>
          ZIO
            .fromAutoCloseable(ZIO.succeed(new java.io.InputStreamReader(is)))
            .map(isr => new zio.json.internal.WithRetractReader(isr))
      fullDoc <- normalizeJson(in)
    yield fullDoc

  private def normalizeJson(in: RetractReader): ResourceStream[Json] =
    val trace: List[JsonError] = Nil
    val K: JsonFieldDecoder[String] = JsonFieldDecoder.string
    val V: JsonDecoder[Json] = Json.decoder

    Lexer.char(trace, in, '{')

    ZStream.paginateZIO[Any, ResourceError, Json, Stack[JsonParsingPhase]](Stack(JsonParsingPhase.Root)):
      parsing =>
        ZIO
          .attemptBlocking:
            val builder = Map.newBuilder[String, Json]

            val currentlyParsing = parsing.top

            // we assume arrays of objects only
            if currentlyParsing == JsonParsingPhase.Array then
              Lexer.char(trace, in, '{')
              parsing.push(JsonParsingPhase.Object)

            var state: (Json, Option[Stack[JsonParsingPhase]]) = (Json.Null, None)
            if Lexer.firstField(trace, in) then

              var continue: Boolean = true

              while continue do
                val field = Lexer.string(trace, in).toString
                val trace_ = JsonError.ObjectAccess(field) :: trace

                Lexer.char(trace_, in, ':')

                val c = in.nextNonWhitespace()
                in.retract()
                (c: @switch) match
                  case '{' =>
                    Lexer.char(trace_, in, '{')
                    state =
                      Json.Obj.apply(builder.result().toList*) -> Some(parsing.push(JsonParsingPhase.Object))
                    continue = state._1 == Json.Obj()
                  case '[' =>
                    Lexer.char(trace_, in, '[')
                    state =
                      Json.Obj.apply(builder.result().toList*) -> Some(parsing.push(JsonParsingPhase.Array))
                    continue = state._1 == Json.Obj()
                  case _ =>
                    val value = V.unsafeDecode(trace_, in)
                    builder += ((K.unsafeDecodeField(trace_, field), value))

                    if !Lexer.nextField(trace, in) then

                      parsing.pop
                      continue = false

                      val json = Json.Obj(builder.result().toList*)
                      val stack = cleanStack(trace, in, parsing)
                      state = json -> (if stack.nonEmpty then Some(stack) else None)

            state
          .catchAll { case t: Throwable =>
            println(s"Error with streaming ${trace.length}")
            trace.foreach(println)
            t.printStackTrace()
            ZIO.fail(ResourceError.SerializationError("Error reading stream", Some(t)))
          }

  @tailrec
  private def cleanStack(
      trace: List[JsonError],
      in: RetractReader,
      stack: Stack[JsonParsingPhase]
  ): Stack[JsonParsingPhase] =
    if stack.nonEmpty then
      stack.pop match
        case JsonParsingPhase.Root =>
          if Lexer.nextField(trace, in) then
            stack.push(JsonParsingPhase.Root)
            stack
          else stack
        case JsonParsingPhase.Array =>
          if Lexer.nextArrayElement(trace, in) then
            stack.push(JsonParsingPhase.Array)
            stack
          else cleanStack(trace, in, stack)
        case JsonParsingPhase.Object =>
          if Lexer.nextField(trace, in) then
            stack.push(JsonParsingPhase.Object)
            stack
          else cleanStack(trace, in, stack)
    else stack
