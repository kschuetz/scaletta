package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.runtime.VarAddress

private[scaletta] final class CapturePlan(val signature: CaptureSignature,
                                          val sourceIndices: Array[Int],
                                          val targetEncoded: Array[Int]) {
  def capture(source: VarSpace, target: CapturedFrame): Unit = {
    var i = 0
    while (i < sourceIndices.length) {
      val sourceIndex = sourceIndices(i)
      val encoded = targetEncoded(i)
      val typeTag = VarAddress.decodeBasicType(encoded)
      val targetOffset = VarAddress.decodeStackOffset(encoded)

      (typeTag: @annotation.switch) match {
        case BasicTypes.Object => target.objects(targetOffset) = source.unsafeReadObject(sourceIndex)
        case BasicTypes.Boolean => target.booleans(targetOffset) = source.unsafeReadBoolean(sourceIndex)
        case BasicTypes.Int => target.ints(targetOffset) = source.unsafeReadInt(sourceIndex)
        case BasicTypes.Long => target.longs(targetOffset) = source.unsafeReadLong(sourceIndex)
        case BasicTypes.Short => target.shorts(targetOffset) = source.unsafeReadShort(sourceIndex)
        case BasicTypes.Byte => target.bytes(targetOffset) = source.unsafeReadByte(sourceIndex)
        case BasicTypes.Char => target.chars(targetOffset) = source.unsafeReadChar(sourceIndex)
        case BasicTypes.Double => target.doubles(targetOffset) = source.unsafeReadDouble(sourceIndex)
        case BasicTypes.Float => target.floats(targetOffset) = source.unsafeReadFloat(sourceIndex)
      }
      i += 1
    }
  }
}
