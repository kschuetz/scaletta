package software.kes.scaletta.interpreter

trait OpcodeWriter {
  def currentAddress: Int

  def write(address: Int, data: Int): Unit

  def writeAndAdvance(opcodeOrOperand: Int): Unit
}
