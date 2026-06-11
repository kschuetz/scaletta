package software.kes.scaletta.internal.interpreter

import scala.language.implicitConversions

object Initializer {
  def apply(fn: VarSpace => Unit): Initializer =
    fromFunction(fn)

  implicit def fromFunction(fn: VarSpace => Unit): Initializer =
    (input: VarSpace) => fn(input)

  object none extends Initializer {
    def apply(input: VarSpace): Unit = ()
  }
}

trait Initializer extends (VarSpace => Unit)
