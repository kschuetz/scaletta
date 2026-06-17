package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.internal.runtime.UserFunctionSignature
import software.kes.scaletta.util.array.ArrayUtil

import scala.collection.immutable.ArraySeq

object UserFunctionBuilder {
  def create(signature: UserFunctionSignature): UserFunctionBuilder = {
    new UserFunctionBuilder(signature, new Array[Int](16), 0)
  }
}

final class UserFunctionBuilder private(private val signature: UserFunctionSignature,
                                        private var buffer: Array[Int],
                                        private var writePtr: Int) extends OpcodeWriter {

  def currentAddress: Int = writePtr

  def write(address: Int, data: Int, mask: Int = 0xFFFFFFFF): Unit = {
    if (address < 0 || address >= writePtr) {
      throw new IndexOutOfBoundsException(s"Address $address is out of bounds (0 to ${writePtr - 1})")
    }
    if (mask == 0xFFFFFFFF) {
      buffer(address) = data
    } else {
      buffer(address) = (buffer(address) & ~mask) | (data & mask)
    }
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
