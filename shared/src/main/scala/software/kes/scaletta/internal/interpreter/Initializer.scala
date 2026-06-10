package software.kes.scaletta.internal.interpreter

import scala.language.implicitConversions

object Initializer {
  def apply(fn: VarSpace => VarSpace): Initializer =
    fromFunction(fn)

  implicit def fromFunction(fn: VarSpace => VarSpace): Initializer =
    (input: VarSpace) => fn(input)

  object none extends Initializer {
    def apply(input: VarSpace): VarSpace = input
  }
}

trait Initializer extends (VarSpace => VarSpace)
