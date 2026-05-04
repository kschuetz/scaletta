package software.kes.scaletta.internal.runtime

import software.kes.scaletta.internal.types.{Type, TypeId}

object CoreTypes {
  final val AnyT: Type.Nominal[TypeId] = Type.Nominal(TypeId(1))
  final val AnyValT: Type.Nominal[TypeId] = Type.Nominal(TypeId(2))
  final val AnyRefT: Type.Nominal[TypeId] = Type.Nominal(TypeId(3))
  final val NullT: Type.Nominal[TypeId] = Type.Nominal(TypeId(4))
  final val NothingT: Type.Nominal[TypeId] = Type.Nominal(TypeId(5))
  final val UnitT: Type.Nominal[TypeId] = Type.Nominal(TypeId(6))
  final val BooleanT: Type.Nominal[TypeId] = Type.Nominal(TypeId(7))
  final val ByteT: Type.Nominal[TypeId] = Type.Nominal(TypeId(8))
  final val CharT: Type.Nominal[TypeId] = Type.Nominal(TypeId(9))
  final val DoubleT: Type.Nominal[TypeId] = Type.Nominal(TypeId(10))
  final val FloatT: Type.Nominal[TypeId] = Type.Nominal(TypeId(11))
  final val IntT: Type.Nominal[TypeId] = Type.Nominal(TypeId(12))
  final val LongT: Type.Nominal[TypeId] = Type.Nominal(TypeId(13))
  final val ShortT: Type.Nominal[TypeId] = Type.Nominal(TypeId(14))
  final val StringT: Type.Nominal[TypeId] = Type.Nominal(TypeId(15))

  private[scaletta] val Max: Int = StringT.name.value
}
