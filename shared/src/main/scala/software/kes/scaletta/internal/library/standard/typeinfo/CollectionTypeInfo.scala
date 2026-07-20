package software.kes.scaletta.internal.library.standard.typeinfo

import software.kes.scaletta.api.RuntimeTypeInfo
import software.kes.scaletta.api.UnapplyStrategy.unapplySeq

object CollectionTypeInfo {
  val ListT = RuntimeTypeInfo(_.isInstanceOf[List[_]],
    unapplySeq(_.isInstanceOf[List[_]]))

  val VectorT = RuntimeTypeInfo(_.isInstanceOf[Vector[_]],
    unapplySeq(_.isInstanceOf[Vector[_]]))
}
