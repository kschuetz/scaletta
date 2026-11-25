package software.kes.scaletta.internal.interpreter.testsupport

import software.kes.scaletta.api.StringFormatterErrorHandler
import software.kes.scaletta.internal.interpreter.InterpreterFactory

object TestInterpreterFactory extends PlatformTestInterpreterFactory {
  private val stringFormatterErrorHandler = StringFormatterErrorHandler.default

  def create(): InterpreterFactory =
    InterpreterFactory.create(valueFormatter, stringFormatterErrorHandler)
}
