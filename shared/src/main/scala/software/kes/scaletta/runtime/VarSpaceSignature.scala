package software.kes.scaletta.runtime

import software.kes.scaletta.common.BasicTypes

import scala.collection.immutable.ArraySeq

object VarSpaceSignature {
  def of(frames: FrameSignature*): VarSpaceSignature =
    fromSeq(frames)

  def fromSeq(frames: Iterable[FrameSignature]): VarSpaceSignature = {
    val size = frames.foldLeft(0) {
      case (acc, frame) => acc + frame.slotCount
    }
    if (size < 1) empty
    else {
      val out = new Array[VarAddress.Encoded](size)
      val occ = Array.fill[Int](BasicTypes.MaxValue + 1)(0)
      var i = 0
      frames.foreach { frame =>
        frame.slots.foreach { slot =>
          val t = VarAddress.decodeBasicType(slot)
          val occCount = occ(t)
          occ(t) += 1
          out(i) = VarAddress.encode(t, occCount)
          i += 1
        }

      }
      new VarSpaceSignature(ArraySeq.unsafeWrapArray(out))
    }
  }

  val empty: VarSpaceSignature = new VarSpaceSignature(ArraySeq.empty)
}

final class VarSpaceSignature private(val slots: ArraySeq[VarAddress.Encoded]) {
  def slotCount: Int = slots.length

  def slot(index: Int): VarAddress.Encoded = slots(index)

  def basicTypeOf(index: Int): Byte = VarAddress.decodeBasicType(slots(index))

  def stackOffsetOf(index: Int): Int = VarAddress.decodeStackOffset(slots(index))

  override def equals(other: Any): Boolean = other match {
    case that: VarSpaceSignature =>
      slots == that.slots
    case _ => false
  }

  override def hashCode(): Int = slots.hashCode()

  override def toString: String =
    slots.map(p => BasicTypes.friendlyName(VarAddress.decodeBasicType(p)))
      .mkString("VarSpaceSignature(", ", ", ")")
}
