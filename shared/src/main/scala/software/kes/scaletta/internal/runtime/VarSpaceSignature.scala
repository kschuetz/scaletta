package software.kes.scaletta.internal.runtime

import software.kes.scaletta.common.{BasicType, BasicTypes}

import scala.collection.immutable.ArraySeq

object VarSpaceSignature {
  def of(frames: FrameSignature*): VarSpaceSignature =
    fromSeq(frames)

  def fromSeq(frames: Iterable[FrameSignature]): VarSpaceSignature = {
    val size = frames.foldLeft(0) {
      case (acc, frame) => acc + frame.slotCount
    }

    val firstFrame = frames.headOption.getOrElse(FrameSignature.empty)

    if (size < 1) {
      new VarSpaceSignature(ArraySeq.empty, FrameSignature.empty)
    } else {
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
      new VarSpaceSignature(ArraySeq.unsafeWrapArray(out), firstFrame)
    }
  }

  def create(slots: ArraySeq[VarAddress.Encoded],
             frameSignature: FrameSignature): VarSpaceSignature = {
    new VarSpaceSignature(slots, frameSignature)
  }

  val empty: VarSpaceSignature = new VarSpaceSignature(ArraySeq.empty, FrameSignature.empty)
}

final class VarSpaceSignature private(val slots: ArraySeq[VarAddress.Encoded],
                                      val frameSignature: FrameSignature) {
  def slotCount: Int = slots.length

  def slot(index: Int): VarAddress.Encoded = slots(index)

  def basicTypeOf(index: Int): BasicType = VarAddress.decodeBasicType(slots(index))

  def stackOffsetOf(index: Int): Int = VarAddress.decodeStackOffset(slots(index))

  def pushFrame(frame: FrameSignature): VarSpaceSignature = {
    val newSize = frame.slotCount + this.slotCount
    val newSlots = new Array[VarAddress.Encoded](newSize)

    var i = 0
    while (i < frame.slotCount) {
      newSlots(i) = frame.slot(i)
      i += 1
    }

    var j = 0
    while (j < this.slotCount) {
      val originalEncoded = this.slots(j)
      val t = VarAddress.decodeBasicType(originalEncoded)
      val originalOffset = VarAddress.decodeStackOffset(originalEncoded)
      val shift = frame.countFor(t)

      newSlots(i) = VarAddress.encode(t, originalOffset + shift)
      i += 1
      j += 1
    }

    new VarSpaceSignature(ArraySeq.unsafeWrapArray(newSlots), frame)
  }

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
