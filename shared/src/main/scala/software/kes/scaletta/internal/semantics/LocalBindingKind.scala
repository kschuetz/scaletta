package software.kes.scaletta.internal.semantics

sealed trait LocalBindingKind

object LocalBindingKind {
  case object Val extends LocalBindingKind

  case object LazyVal extends LocalBindingKind

  case object Def extends LocalBindingKind

  case object Parameter extends LocalBindingKind

  case object PatternBinding extends LocalBindingKind
}
