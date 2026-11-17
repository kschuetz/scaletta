package software.kes.scaletta.internal.ast

import software.kes.scaletta.api.{NativeFunctionId, QualifiedName}

sealed trait SymbolId

sealed trait TypeSymbolId extends SymbolId

sealed trait TermSymbolId extends SymbolId

object TermSymbolId {
  final case class Static(name: QualifiedName.Full) extends TermSymbolId

  final case class Local(name: String, depth: Int, slot: Int) extends TermSymbolId
}

case class NativeFunctionSymbolId(value: NativeFunctionId) extends SymbolId
