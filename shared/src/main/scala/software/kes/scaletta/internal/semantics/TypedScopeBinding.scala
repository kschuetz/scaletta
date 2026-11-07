package software.kes.scaletta.internal.semantics

import software.kes.scaletta.api.{ProperType, TypeId}

final case class TypedScopeBinding(name: String,
                                   slot: Int,
                                   kind: LocalBindingKind,
                                   tpe: ProperType[TypeId])
