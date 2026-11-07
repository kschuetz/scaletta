package software.kes.scaletta.internal.ast

import software.kes.scaletta.api.{ProperType, TypeId}
import software.kes.scaletta.internal.builtins.NativeFunctionDefinition
import software.kes.scaletta.internal.semantics.{LocalBindingKind, SemanticError}

sealed trait TypedInfo {
  def properType: Option[ProperType[TypeId]]
}

object TypedInfo {
  final case class Local(depth: Int,
                         slot: Int,
                         name: String,
                         kind: LocalBindingKind,
                         tpe: ProperType[TypeId]) extends TypedInfo {
    def properType: Option[ProperType[TypeId]] = Some(tpe)
  }

  final case class Bound(symbol: SymbolId,
                         tpe: ProperType[TypeId]) extends TypedInfo {
    def properType: Option[ProperType[TypeId]] = Some(tpe)
  }

  final case class MethodCall(target: NativeFunctionDefinition,
                              tpe: ProperType[TypeId]) extends TypedInfo {
    def properType: Option[ProperType[TypeId]] = Some(tpe)
  }

  final case class StaticFunctionCall(target: NativeFunctionDefinition,
                                      tpe: ProperType[TypeId]) extends TypedInfo {
    def properType: Option[ProperType[TypeId]] = Some(tpe)
  }

  final case class FieldAccess(name: String,
                               tpe: ProperType[TypeId]) extends TypedInfo {
    def properType: Option[ProperType[TypeId]] = Some(tpe)
  }

  final case class Error(error: SemanticError) extends TypedInfo {
    def properType: Option[ProperType[TypeId]] = None
  }
}
