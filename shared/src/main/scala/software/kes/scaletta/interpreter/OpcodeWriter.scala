package software.kes.scaletta.interpreter

trait OpcodeWriter {
  /**
   * Writes an opcode or operand to the current address and advances the address.
   */
  def writeAndAdvance(opcodeOrOperand: Int): Unit

  /**
   * The address of the next instruction to be written.
   */
  def currentAddress: Int

  /**
   * Writes an opcode or operand to the specified address. The address must be
   * less than the value returned by currentAddress, or an exception will be thrown.
   */
  def write(address: Int, data: Int): Unit
}
