package software.kes.scaletta.interpreter

import software.kes.scaletta.runtime.VarSpaceSignature
import software.kes.scaletta.util.array.ArrayUtil

import scala.collection.immutable.ArraySeq

object UserFunctionBuilder {
  def create(signature: VarSpaceSignature): UserFunctionBuilder = {
    new UserFunctionBuilder(signature, new Array[Int](16), 0)
  }
}

final class UserFunctionBuilder private(private val signature: VarSpaceSignature,
                                        private var buffer: Array[Int],
                                        private var writePtr: Int) extends OpcodeWriter {

  def currentAddress: Int = writePtr

  def write(address: Int, data: Int): Unit = {
    if (address < 0 || address >= writePtr) {
      throw new IndexOutOfBoundsException(s"Address $address is out of bounds (0 to ${writePtr - 1})")
    }
    buffer(address) = data
  }

  def writeAndAdvance(opcodeOrOperand: Int): Unit = {
    ensureCapacity(writePtr + 1)
    buffer(writePtr) = opcodeOrOperand
    writePtr += 1
  }

  def build(): UserFunction = {
    val instructions = ArraySeq.unsafeWrapArray(buffer.take(writePtr))
    UserFunction(signature, instructions)
  }

  private def ensureCapacity(minCapacity: Int): Unit = {
    buffer = ArrayUtil.growIntArray(buffer, minCapacity, writePtr)
  }
}
