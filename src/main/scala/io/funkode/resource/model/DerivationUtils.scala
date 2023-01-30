/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.compiletime.*
import scala.quoted.*

object DerivationUtils:

  inline def getTypeStringFromTuple[T <: Tuple]: List[String] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => getTypeString[t] :: getTypeStringFromTuple[ts]

  inline def getTypeString[A]: String = ${ getTypeStringImpl[A] }

  def getTypeStringImpl[A: Type](using Quotes): Expr[String] =
    Expr(Type.show[A])

  inline def deriveMirrorElementNames[T <: Tuple]: List[Any] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts)  => typeName[t](using summonInline[ValueOf[t]]) :: deriveMirrorElementNames[ts]

  inline def typeName[R](using ValueOf[R]): R = valueOf[R]
