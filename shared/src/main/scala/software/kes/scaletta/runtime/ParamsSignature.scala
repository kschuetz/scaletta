package software.kes.scaletta.runtime

import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.types.{Type, TypeId}

import scala.collection.immutable.ArraySeq

object ParamsSignature {
  def of(types: Type[TypeId]*): ParamsSignature =
    fromSeq(types)

  def fromSeq(params: Iterable[Type[TypeId]]): ParamsSignature =
    new ParamsSignature(ArraySeq.unsafeWrapArray(params.map(BasicTypes.fromType).toArray))

  val empty: ParamsSignature = new ParamsSignature(ArraySeq.empty)
}

final class ParamsSignature private(val params: ArraySeq[Byte]) {
  override def equals(other: Any): Boolean = other match {
    case that: ParamsSignature =>
      params == that.params
    case _ => false
  }

  override def hashCode(): Int = params.hashCode()

  override def toString: String =
    params.map(BasicTypes.friendlyName)
      .mkString("ParamsSignature(", ", ", ")")
}
