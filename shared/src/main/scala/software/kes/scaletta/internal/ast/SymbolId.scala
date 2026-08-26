package software.kes.scaletta.internal.ast

sealed trait SymbolId

sealed trait TypeSymbolId extends SymbolId

sealed trait TermSymbolId extends SymbolId

case class NativeFunctionId(value: Long) extends SymbolId
