package software.kes.scaletta.internal.symbols

import software.kes.scaletta.api.{Type, TypeId}

import scala.language.implicitConversions

sealed trait SignatureQueryParameter

object SignatureQueryParameter {
  implicit def ofType(typ: Type[TypeId]): SignatureQueryParameter = OfType(typ)

  val unknown: SignatureQueryParameter = Unknown

  case class OfType(typ: Type[TypeId]) extends SignatureQueryParameter

  case object Unknown extends SignatureQueryParameter
}
