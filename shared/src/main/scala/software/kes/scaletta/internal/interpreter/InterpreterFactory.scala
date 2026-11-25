package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.ValueFormatterErrorHandler
import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.format.ValueFormatter

object InterpreterFactory {
  def create(valueFormatter: ValueFormatter,
             valueFormatterErrorHandler: ValueFormatterErrorHandler): InterpreterFactory =
    new InterpreterFactory(valueFormatter, valueFormatterErrorHandler)
}

final class InterpreterFactory private(valueFormatter: ValueFormatter,
                                       valueFormatterErrorHandler: ValueFormatterErrorHandler) {
  def create(program: Program,
             functionTable: NativeFunctionTable): Interpreter =
    Interpreter.create(program, functionTable, valueFormatter, valueFormatterErrorHandler)

  def withValueFormatter(newFormatter: ValueFormatter): InterpreterFactory =
    new InterpreterFactory(newFormatter, valueFormatterErrorHandler)

  def withValueFormatterErrorHandler(newErrorHandler: ValueFormatterErrorHandler): InterpreterFactory =
    new InterpreterFactory(valueFormatter, newErrorHandler)
}
