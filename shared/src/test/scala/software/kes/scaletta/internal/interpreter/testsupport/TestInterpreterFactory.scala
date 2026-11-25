package software.kes.scaletta.internal.interpreter.testsupport

import software.kes.scaletta.api.ValueFormatterErrorHandler
import software.kes.scaletta.internal.interpreter.InterpreterFactory

object TestInterpreterFactory extends PlatformTestInterpreterFactory {
  private val valueFormatterErrorHandler = ValueFormatterErrorHandler.default

  def create(): InterpreterFactory =
    InterpreterFactory.create(valueFormatter, valueFormatterErrorHandler)
}
