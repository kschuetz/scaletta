package software.kes.scaletta.runtime

object ParamAddress {
  type Encoded = Int

  def decodeBasicType(encoded: Encoded): Byte =
    ((encoded >> 24) & 0xff).toByte

  def decodeStackOffset(encoded: Encoded): Int =
    encoded & 0xffffff

  def encode(basicType: Byte, stackOffset: Int): Encoded =
    ((basicType & 0xff) << 24) | (stackOffset & 0xffffff)
}
