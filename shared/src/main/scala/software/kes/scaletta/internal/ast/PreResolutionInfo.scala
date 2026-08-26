package software.kes.scaletta.internal.ast

sealed trait PreResolutionInfo

object PreResolutionInfo {
  // Fully bound term or type symbol (no type info required to bind)
  case class Bound(symbol: SymbolId) extends PreResolutionInfo

  // Name that could not be bound in this phase
  case class Unresolved(name: String) extends PreResolutionInfo

  // Overloadable/receiver-sensitive sites that need types later
  case class MethodSite(name: String,
                        argGroupArity: Vector[Int]) extends PreResolutionInfo

  case class StaticFunctionSite(qualifiedName: String,
                                argGroupArity: Vector[Int]) extends PreResolutionInfo

  case class FieldSite(name: String) extends PreResolutionInfo

  case class AmbiguousName(name: String, candidateCount: Int) extends PreResolutionInfo
}
