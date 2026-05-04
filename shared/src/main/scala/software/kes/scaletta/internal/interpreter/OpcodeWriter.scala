package software.kes.scaletta.internal.interpreter

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
   *
   * Bits where mask is 1 will be updated with bits from data.
   * Bits where mask is 0 will be preserved from the existing value.
   */
  def write(address: Int, data: Int, mask: Int = 0xFFFFFFFF): Unit
}
