package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.StringFormatterErrorHandler
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.format.ValueFormatter

object InterpreterFactory {
  def create(valueFormatter: ValueFormatter,
             stringFormatterErrorHandler: StringFormatterErrorHandler): InterpreterFactory =
    new InterpreterFactory(valueFormatter, stringFormatterErrorHandler)
}

final class InterpreterFactory private(valueFormatter: ValueFormatter,
                                       stringFormatterErrorHandler: StringFormatterErrorHandler) {
  def create(program: Program,
             functionTable: NativeFunctionTable): Interpreter =
    Interpreter.create(program, functionTable)

  def withValueFormatter(newFormatter: ValueFormatter): InterpreterFactory =
    new InterpreterFactory(newFormatter, stringFormatterErrorHandler)

  def withStringFormatterErrorHandler(newErrorHandler: StringFormatterErrorHandler): InterpreterFactory =
    new InterpreterFactory(valueFormatter, newErrorHandler)
}
