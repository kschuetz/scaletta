package software.kes.scaletta.runtime

import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.types.{Type, TypeId}

import scala.collection.immutable.ArraySeq

object FrameSignature {
  def of(types: Type[TypeId]*): FrameSignature =
    fromSeq(types)

  def fromSeq(params: Iterable[Type[TypeId]]): FrameSignature = {
    val types = params.map(BasicTypes.fromType).toArray
    val occ = Array.fill[Int](BasicTypes.MaxValue + 1)(0)
    val out = new Array[VarAddress.Encoded](params.size)
    var i = 0
    while (i < types.length) {
      val t = types(i)
      val occCount = occ(t)
      occ(t) += 1
      out(i) = VarAddress.encode(t, occCount)
      i += 1
    }
    new FrameSignature(ArraySeq.unsafeWrapArray(out))
  }

  val empty: FrameSignature = new FrameSignature(ArraySeq.empty)
}

/**
 * This is intended for interaction with local functions. It used to create a VarSpace from
 * the variable stack.
 *
 * Each slot is encoded as a pair of a basic type and a stack offset into the specialized stack
 * for that basic type.
 */
final class FrameSignature private(val slots: ArraySeq[VarAddress.Encoded]) {
  def slotCount: Int = slots.length

  def slot(index: Int): VarAddress.Encoded = slots(index)

  def basicTypeOf(index: Int): Byte = VarAddress.decodeBasicType(slots(index))

  def stackOffsetOf(index: Int): Int = VarAddress.decodeStackOffset(slots(index))

  override def equals(other: Any): Boolean = other match {
    case that: FrameSignature =>
      slots == that.slots
    case _ => false
  }

  override def hashCode(): Int = slots.hashCode()

  override def toString: String =
    slots.map(p => BasicTypes.friendlyName(VarAddress.decodeBasicType(p)))
      .mkString("FrameSignature(", ", ", ")")
}
