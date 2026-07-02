package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.{Name, ProperType, Type, TypeId}

object StandardTypes {
  object names {
    val AnyRefT = Name("AnyRef")
    val AnyT = Name("Any")
    val AnyValT = Name("AnyVal")
    val BooleanT = Name("Boolean")
    val ByteT = Name("Byte")
    val CharT = Name("Char")
    val ConsT = Name("::")
    val DoubleT = Name("Double")
    val FloatT = Name("Float")
    val IntT = Name("Int")
    val ListT = Name("List")
    val LongT = Name("Long")
    val MapT = Name("Map")
    val NilT = Name("Nil")
    val NoneT = Name("None")
    val NothingT = Name("Nothing")
    val NullT = Name("Null")
    val OptionT = Name("Option")
    val SetT = Name("Set")
    val ShortT = Name("Short")
    val SomeT = Name("Some")
    val StringT = Name("String")
    val UnitT = Name("Unit")
    val VectorT = Name("Vector")
  }
}

trait StandardTypes {
  def AnyRefT: ProperType[TypeId]

  def AnyT: ProperType[TypeId]

  def AnyValT: ProperType[TypeId]

  def BooleanT: Type.Nominal[TypeId]

  def ByteT: Type.Nominal[TypeId]

  def CharT: Type.Nominal[TypeId]

  def ConsT: Type.Constructor[TypeId]

  def DoubleT: Type.Nominal[TypeId]

  def FloatT: Type.Nominal[TypeId]

  def IntT: Type.Nominal[TypeId]

  def ListT: Type.Constructor[TypeId]

  def LongT: Type.Nominal[TypeId]

  def MapT: Type.Constructor[TypeId]

  def NilT: Type.Applied[TypeId]

  def NoneT: Type.Applied[TypeId]

  def NothingT: ProperType[TypeId]

  def NullT: ProperType[TypeId]

  def OptionT: Type.Constructor[TypeId]

  def SetT: Type.Constructor[TypeId]

  def ShortT: Type.Nominal[TypeId]

  def SomeT: Type.Constructor[TypeId]

  def StringT: Type.Nominal[TypeId]

  def UnitT: ProperType[TypeId]

  def VectorT: Type.Constructor[TypeId]
}
