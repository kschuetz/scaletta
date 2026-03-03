package software.kes.scaletta.ast

sealed trait Binding

object Binding {
  case class Val(name: Identifier,
                 rhs: Expression) extends Binding

  case class Def(name: Identifier,
                 params: Vector[FormalParameterGroup],
                 body: Expression) extends Binding

  case class LazyVal(name: Identifier,
                     rhs: Expression) extends Binding
}
