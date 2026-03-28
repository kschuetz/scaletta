package software.kes.scaletta.interpreter

trait OpcodeEmitter {
  def emit(opcodeOrOperand: Int): Unit
}
