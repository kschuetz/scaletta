package software.kes.scaletta.internal.ast

trait PhaseWithLinkedInfo extends Phase {
  type Ident = LinkedInfo
}
