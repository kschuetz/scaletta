package software.kes.scaletta.internal.library.standard.typeinfo

import software.kes.scaletta.api.UnapplyStrategy.{unapplySeq, unapplyTwo, unapplyZero}
import software.kes.scaletta.api.{RuntimeTypeInfo, UnapplyResult}

object CollectionTypeInfo {
  final val ListT = RuntimeTypeInfo(_.isInstanceOf[List[_]],
    unapplySeq(_.isInstanceOf[List[_]]))

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
}
