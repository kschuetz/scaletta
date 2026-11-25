package software.kes.scaletta.internal.interpreter.testsupport

import software.kes.scaletta.internal.interpreter.InterpreterFactory

object TestInterpreterFactory extends PlatformTestInterpreterFactory {
  def create(): InterpreterFactory = InterpreterFactory.create(valueFormatter)
}
