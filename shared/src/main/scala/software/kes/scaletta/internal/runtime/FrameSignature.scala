package software.kes.scaletta.internal.runtime

import software.kes.scaletta.api.{Type, TypeId}
import software.kes.scaletta.common.BasicTypes

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
    new FrameSignature(
      ArraySeq.unsafeWrapArray(out),
      occ(BasicTypes.Object),
      occ(BasicTypes.Boolean),
      occ(BasicTypes.Int),
      occ(BasicTypes.Long),
      occ(BasicTypes.Short),
      occ(BasicTypes.Byte),
      occ(BasicTypes.Char),
      occ(BasicTypes.Double),
      occ(BasicTypes.Float)
    )
  }

  val empty: FrameSignature = new FrameSignature(ArraySeq.empty, 0, 0, 0, 0, 0, 0, 0, 0, 0)
}

/**
 * This is intended for interaction with local functions. It used to create a VarSpace from
 * the variable stack.
 *
 * Each slot is encoded as a pair of a basic type and a stack offset into the specialized stack
 * for that basic type.
 */
final class FrameSignature private(val slots: ArraySeq[VarAddress.Encoded],
                                   val objectCount: Int,
                                   val booleanCount: Int,
                                   val intCount: Int,
                                   val longCount: Int,
                                   val shortCount: Int,
                                   val byteCount: Int,
                                   val charCount: Int,
                                   val doubleCount: Int,
                                   val floatCount: Int) {
  def slotCount: Int = slots.length

  def slot(index: Int): VarAddress.Encoded = slots(index)

  def basicTypeOf(index: Int): Byte = VarAddress.decodeBasicType(slots(index))

  def stackOffsetOf(index: Int): Int = VarAddress.decodeStackOffset(slots(index))

  def countFor(t: Byte): Int =
    t match {
      case BasicTypes.Object => objectCount
      case BasicTypes.Boolean => booleanCount
      case BasicTypes.Int => intCount
      case BasicTypes.Long => longCount
      case BasicTypes.Short => shortCount
      case BasicTypes.Byte => byteCount
      case BasicTypes.Char => charCount
      case BasicTypes.Double => doubleCount
      case BasicTypes.Float => floatCount
      case _ => 0
    }

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
