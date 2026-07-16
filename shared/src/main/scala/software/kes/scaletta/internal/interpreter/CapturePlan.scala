package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.runtime.VarAddress

import scala.collection.immutable.ArraySeq

object CapturePlan {
  def create(signature: CaptureSignature,
             sourceIndices: ArraySeq[Int],
             targetEncoded: ArraySeq[Int]): CapturePlan =
    if (signature.isEmpty) empty else new CapturePlan(signature, sourceIndices, targetEncoded)

  val empty: CapturePlan = new CapturePlan(CaptureSignature.empty, ArraySeq.empty, ArraySeq.empty)
}

private[scaletta] final class CapturePlan private(val signature: CaptureSignature,
                                                  sourceIndices: ArraySeq[Int],
                                                  targetEncoded: ArraySeq[Int]) {
  def isEmpty: Boolean = signature.isEmpty

  def capture(source: VarSpace, target: CapturedFrame): Unit = {
    if (!signature.isEmpty) {
      var i = 0
      while (i < sourceIndices.length) {
        val sourceIndex = sourceIndices(i)
        val encoded = targetEncoded(i)
        val typeTag = VarAddress.decodeBasicType(encoded)
        val targetOffset = VarAddress.decodeStackOffset(encoded)

        (typeTag: @annotation.switch) match {
          case BasicTypes.Boolean => target.booleans(targetOffset) = source.unsafeReadBoolean(sourceIndex)
          case BasicTypes.Int => target.ints(targetOffset) = source.unsafeReadInt(sourceIndex)
          case BasicTypes.Long => target.longs(targetOffset) = source.unsafeReadLong(sourceIndex)
          case BasicTypes.Short => target.shorts(targetOffset) = source.unsafeReadShort(sourceIndex)
          case BasicTypes.Byte => target.bytes(targetOffset) = source.unsafeReadByte(sourceIndex)
          case BasicTypes.Char => target.chars(targetOffset) = source.unsafeReadChar(sourceIndex)
          case BasicTypes.Double => target.doubles(targetOffset) = source.unsafeReadDouble(sourceIndex)
          case BasicTypes.Float => target.floats(targetOffset) = source.unsafeReadFloat(sourceIndex)
          case _ => target.objects(targetOffset) = source.unsafeReadObject(sourceIndex)
        }
        i += 1
      }
    }
  }
}
