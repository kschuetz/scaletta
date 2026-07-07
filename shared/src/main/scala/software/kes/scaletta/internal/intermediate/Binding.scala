package software.kes.scaletta.internal.intermediate

import software.kes.scaletta.internal.runtime.UserFunctionSignature

sealed trait Binding

object Binding {
  case class Val(value: IntermediateExpression) extends Binding

  case class LazyVal(value: IntermediateExpression) extends Binding

  case class Def(signature: UserFunctionSignature,
                 body: IntermediateExpression) extends Binding
}
