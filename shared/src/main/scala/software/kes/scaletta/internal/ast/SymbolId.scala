package software.kes.scaletta.internal.ast

import software.kes.scaletta.api.NativeFunctionId

sealed trait SymbolId

sealed trait TypeSymbolId extends SymbolId

sealed trait TermSymbolId extends SymbolId

case class NativeFunctionSymbolId(value: NativeFunctionId) extends SymbolId
