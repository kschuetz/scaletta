package software.kes.scaletta.internal.ast

trait PhaseWithTypedInfo extends Phase {
  type Ident = TypedInfo
}
