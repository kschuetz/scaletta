package software.kes.scaletta.runtime

import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.types.{Type, TypeId}

import scala.collection.immutable.ArraySeq

object ParamsSignature {
  def of(types: Type[TypeId]*): ParamsSignature =
    fromSeq(types)

  def fromSeq(params: Iterable[Type[TypeId]]): ParamsSignature = {
    val types = params.map(BasicTypes.fromType).toArray
    val occ = Array.fill[Int](BasicTypes.MaxValue + 1)(0)
    val out = new Array[ParamAddress.Encoded](params.size)
    var i = types.length - 1
    while (i >= 0) {
      val t = types(i)
      val occCount = occ(t)
      occ(t) += 1
      out(i) = ParamAddress.encode(t, occCount)
      i -= 1
    }
    new ParamsSignature(ArraySeq.unsafeWrapArray(out))
  }

  val empty: ParamsSignature = new ParamsSignature(ArraySeq.empty)
}

/**
 * Each parameter is encoded as a pair of a basic type and a stack offset into the specialized stack
 * for that basic type.
 * This assumes that arguments were pushed onto the stack from left to right.
 * (i.e., the top of the stack will contain the last argument)
 */
final class ParamsSignature private(val params: ArraySeq[ParamAddress.Encoded]) {
  def paramCount: Int = params.length

  def param(index: Int): ParamAddress.Encoded = params(index)

  def basicTypeOf(index: Int): Byte = ParamAddress.decodeBasicType(params(index))

  def stackOffsetOf(index: Int): Int = ParamAddress.decodeStackOffset(params(index))

  override def equals(other: Any): Boolean = other match {
    case that: ParamsSignature =>
      params == that.params
    case _ => false
  }

  override def hashCode(): Int = params.hashCode()

  override def toString: String =
    params.map(p => BasicTypes.friendlyName(ParamAddress.decodeBasicType(p)))
      .mkString("ParamsSignature(", ", ", ")")
}
