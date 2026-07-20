package software.kes.scaletta.internal.library.standard.typeinfo

import software.kes.scaletta.api.UnapplyStrategy.{unapplyOne, unapplyZero}
import software.kes.scaletta.api.{RuntimeTypeInfo, UnapplyResult}

object OptionTypeInfo {
  val SomeT = RuntimeTypeInfo(_.isInstanceOf[Some[_]],
    unapplyOne {
      case Some(x) => UnapplyResult.success1(x)
      case _ => UnapplyResult.failure
    })

  val NoneT = RuntimeTypeInfo(_ == None,
    unapplyZero {
      case None => true
      case _ => false
    })

}
