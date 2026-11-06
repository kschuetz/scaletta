package software.kes.scaletta.internal.ast

import software.kes.scaletta.internal.semantics.{LocalBindingKind, SemanticError}

sealed trait LinkedInfo

object LinkedInfo {
  final case class Local(depth: Int,
                         slot: Int,
                         name: String,
                         kind: LocalBindingKind) extends LinkedInfo

  final case class Bound(symbol: SymbolId) extends LinkedInfo

  final case class MethodSite(name: String,
                              argGroupArity: Vector[Int]) extends LinkedInfo

  final case class StaticFunctionSite(qualifiedName: String,
                                      argGroupArity: Vector[Int]) extends LinkedInfo

  final case class FieldSite(name: String) extends LinkedInfo

  final case class AmbiguousName(name: String, candidateCount: Int) extends LinkedInfo

  final case class Error(error: SemanticError) extends LinkedInfo
}
