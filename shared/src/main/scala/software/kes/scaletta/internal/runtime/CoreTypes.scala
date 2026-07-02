package software.kes.scaletta.internal.runtime

import software.kes.scaletta.api.{ProperType, Type, TypeId}

object CoreTypes {
  final val AnyT: ProperType[TypeId] = Type.top[TypeId]
  final val AnyValT: ProperType[TypeId] = Type.topValue[TypeId]
  final val AnyRefT: ProperType[TypeId] = Type.topRef[TypeId]
  final val BooleanT: Type.Nominal[TypeId] = Type.Nominal(TypeId(4))
  final val ByteT: Type.Nominal[TypeId] = Type.Nominal(TypeId(5))
  final val CharT: Type.Nominal[TypeId] = Type.Nominal(TypeId(6))
  final val DoubleT: Type.Nominal[TypeId] = Type.Nominal(TypeId(7))
  final val FloatT: Type.Nominal[TypeId] = Type.Nominal(TypeId(8))
  final val IntT: Type.Nominal[TypeId] = Type.Nominal(TypeId(9))
  final val LongT: Type.Nominal[TypeId] = Type.Nominal(TypeId(10))
  final val ShortT: Type.Nominal[TypeId] = Type.Nominal(TypeId(11))
  final val StringT: Type.Nominal[TypeId] = Type.Nominal(TypeId(12))

  final val UnitT: ProperType[TypeId] = Type.unit[TypeId]
  final val NullT: ProperType[TypeId] = Type.bottomRef[TypeId]
  final val NothingT: ProperType[TypeId] = Type.bottom[TypeId]

  private[scaletta] val Max: Int = StringT.name.value
}
