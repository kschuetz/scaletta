package software.kes.scaletta.internal.runtime

import software.kes.scaletta.api.{Type, TypeId}
import software.kes.scaletta.common.BasicTypes

import scala.collection.immutable.ArraySeq

object ParamsSignature {
  def of(types: Type[TypeId]*): ParamsSignature =
    fromSeq(types)

  def fromSeq(params: Iterable[Type[TypeId]]): ParamsSignature = {
    val types = params.map(BasicTypes.fromType).toArray
    val counts = new Array[Int](BasicTypes.MaxValue + 1)
    val out = new Array[VarAddress.Encoded](params.size)
    var i = types.length - 1
    while (i >= 0) {
      val t = types(i)
      val occCount = counts(t)
      counts(t) += 1
      out(i) = VarAddress.encode(t, occCount)
      i -= 1
    }
    new ParamsSignature(ArraySeq.unsafeWrapArray(out), ArraySeq.unsafeWrapArray(counts))
  }

  val empty: ParamsSignature = new ParamsSignature(ArraySeq.empty, ArraySeq.fill(BasicTypes.MaxValue + 1)(0))
}

/**
 * This is intended for interaction with native functions. It used to create an ArgumentReader from
 * the operand stack.
 *
 * Each parameter is encoded as a pair of a basic type and a stack offset into the specialized stack
 * for that basic type.
 * This assumes that arguments were pushed onto the stack from left to right.
 * (i.e., the top of the stack will contain the last argument)
 */
final class ParamsSignature private(val params: ArraySeq[VarAddress.Encoded],
                                    val typeCounts: ArraySeq[Int]) {
  def paramCount: Int = params.length

  def param(index: Int): VarAddress.Encoded = params(index)

  def basicTypeOf(index: Int): Byte = VarAddress.decodeBasicType(params(index))

  def stackOffsetOf(index: Int): Int = VarAddress.decodeStackOffset(params(index))

  override def equals(other: Any): Boolean = other match {
    case that: ParamsSignature =>
      params == that.params
    case _ => false
  }

  override def hashCode(): Int = params.hashCode()

  override def toString: String =
    params.map(p => BasicTypes.friendlyName(VarAddress.decodeBasicType(p)))
      .mkString("ParamsSignature(", ", ", ")")
}
