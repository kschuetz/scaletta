package software.kes.scaletta.internal.semantics

final case class ScopeBinding(name: String,
                              slot: Int,
                              kind: LocalBindingKind)
