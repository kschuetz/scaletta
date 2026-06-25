package software.kes.scaletta.internal.runtime

import software.kes.scaletta.api.{Type, TypeId}

object CoreTypes {
  final val AnyT: Type.Nominal[TypeId] = Type.Nominal(TypeId(1))
  final val AnyValT: Type.Nominal[TypeId] = Type.Nominal(TypeId(2))
  final val AnyRefT: Type.Nominal[TypeId] = Type.Nominal(TypeId(3))
  final val BooleanT: Type.Nominal[TypeId] = Type.Nominal(TypeId(4))
  final val ByteT: Type.Nominal[TypeId] = Type.Nominal(TypeId(5))
  final val CharT: Type.Nominal[TypeId] = Type.Nominal(TypeId(6))
  final val DoubleT: Type.Nominal[TypeId] = Type.Nominal(TypeId(7))
  final val FloatT: Type.Nominal[TypeId] = Type.Nominal(TypeId(8))
  final val IntT: Type.Nominal[TypeId] = Type.Nominal(TypeId(9))
  final val LongT: Type.Nominal[TypeId] = Type.Nominal(TypeId(10))
  final val ShortT: Type.Nominal[TypeId] = Type.Nominal(TypeId(11))
  final val StringT: Type.Nominal[TypeId] = Type.Nominal(TypeId(12))

  final val UnitT: Type[TypeId] = Type.Unit
  final val NullT: Type[TypeId] = Type.BottomRef
  final val NothingT: Type[TypeId] = Type.Bottom

  private[scaletta] val Max: Int = StringT.name.value
}
