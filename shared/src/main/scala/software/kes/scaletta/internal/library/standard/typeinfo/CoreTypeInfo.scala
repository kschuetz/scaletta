package software.kes.scaletta.internal.library.standard.typeinfo

import software.kes.scaletta.api.RuntimeTypeInfo
import software.kes.scaletta.util.reflection.Primitives

object CoreTypeInfo {
  final val AnyT = RuntimeTypeInfo(_ => true)
  final val AnyValT = RuntimeTypeInfo(Primitives.isAnyVal)
  final val AnyRefT = RuntimeTypeInfo(_.isInstanceOf[AnyRef])
  final val BooleanT = RuntimeTypeInfo(_.isInstanceOf[Boolean])
  final val ByteT = RuntimeTypeInfo(_.isInstanceOf[Byte])
  final val CharT = RuntimeTypeInfo(_.isInstanceOf[Char])
  final val DoubleT = RuntimeTypeInfo(_.isInstanceOf[Double])
  final val FloatT = RuntimeTypeInfo(_.isInstanceOf[Float])
  final val IntT = RuntimeTypeInfo(_.isInstanceOf[Int])
  final val LongT = RuntimeTypeInfo(_.isInstanceOf[Long])
  final val ShortT = RuntimeTypeInfo(_.isInstanceOf[Short])
  final val StringT = RuntimeTypeInfo(_.isInstanceOf[String])
}
