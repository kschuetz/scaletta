package software.kes.scaletta.internal.runtime

import software.kes.scaletta.common.BasicType

object VarAddress {
  type Encoded = Int

  def decodeBasicType(encoded: Encoded): BasicType =
    ((encoded >> 24) & 0xff).toByte

  def decodeStackOffset(encoded: Encoded): Int =
    encoded & 0xffffff

  def encode(basicType: BasicType, stackOffset: Int): Encoded =
    ((basicType & 0xff) << 24) | (stackOffset & 0xffffff)
}
