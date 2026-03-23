package software.kes.scaletta.spike

import software.kes.scaletta.symbols.Identifier

/**
 * Evaluator will work with this.
 * This will be created after type checking.
 */

sealed trait Expression

object Expression {
  case class Constant(value: Any) extends Expression

  case class If(condition: Expression,
                consequent: Expression,
                alternative: Expression) extends Expression

  // Use a compiled function instead of a name
  case class NativeCall(name: Reference,
                        args: Vector[Expression]) extends Expression

  // Use an ID instead of a name
  case class NativeRef(name: Reference) extends Expression

  // LocalCall and LocalRef for local bindings?

  case class UnaryOperation(operator: UnaryOperator,
                            operand: Expression) extends Expression

  case class BinaryOperation(operation: Operations.Binary,
                             lhs: Expression,
                             rhs: Expression) extends Expression

  // Use an ID instead of a name?
  case class Binding(name: Identifier,
                     value: Any)

  case class LazyBinding(name: Identifier,
                         value: Expression)

  // Use Map with binding IDs instead
  case class WithBindings(bindings: Vector[Binding],
                          body: Expression) extends Expression

  case class Lambda(params: Vector[Identifier],
                    body: Expression) extends Expression
}
