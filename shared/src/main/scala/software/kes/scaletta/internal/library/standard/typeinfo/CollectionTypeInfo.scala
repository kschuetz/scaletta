package software.kes.scaletta.internal.library.standard.typeinfo

import software.kes.scaletta.api.UnapplyStrategy.{unapplySeq, unapplyTwo, unapplyZero}
import software.kes.scaletta.api.{RuntimeTypeInfo, UnapplyResult}
import software.kes.scaletta.internal.library.standard.collection.{ListWithFilter, VectorWithFilter}

object CollectionTypeInfo {
  final val IterableOnceT = RuntimeTypeInfo(_.isInstanceOf[IterableOnce[_]],
    unapplySeq(_.isInstanceOf[IterableOnce[_]]))

  final val ListT = RuntimeTypeInfo(_.isInstanceOf[List[_]],
    unapplySeq(_.isInstanceOf[List[_]]))

  final val ListWithFilterT = RuntimeTypeInfo(_.isInstanceOf[ListWithFilter],
    unapplySeq(_.isInstanceOf[ListWithFilter]))

  final val ConsT = RuntimeTypeInfo(_.isInstanceOf[::[_]],
    unapplyTwo {
      case x :: xs => UnapplyResult.success2(x, xs)
      case _ => UnapplyResult.failure
    })

  final val NilT = RuntimeTypeInfo(_ == Nil,
    unapplyZero {
      case Nil => true
      case _ => false
    })

  final val VectorT = RuntimeTypeInfo(_.isInstanceOf[Vector[_]],
    unapplySeq(_.isInstanceOf[Vector[_]]))

  final val VectorWithFilterT = RuntimeTypeInfo(_.isInstanceOf[VectorWithFilter],
    unapplySeq(_.isInstanceOf[VectorWithFilter]))

  final val SetT = RuntimeTypeInfo(_.isInstanceOf[Set[_]])

  final val MapT = RuntimeTypeInfo(_.isInstanceOf[Map[_, _]])
}
