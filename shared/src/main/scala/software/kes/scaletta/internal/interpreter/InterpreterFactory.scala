package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.internal.builtins.NativeFunctionTable
import software.kes.scaletta.internal.format.ValueFormatter

object InterpreterFactory {
  def create(valueFormatter: ValueFormatter): InterpreterFactory =
    new InterpreterFactory(valueFormatter)
}

final class InterpreterFactory private(valueFormatter: ValueFormatter) {
  def create(program: Program,
             functionTable: NativeFunctionTable): Interpreter =
    Interpreter.create(program, functionTable)
}
