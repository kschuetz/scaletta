package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.internal.symbols.Name
import software.kes.scaletta.types.{Type, TypeConstructor, TypeId}

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
  def AnyRefT: Type.Nominal[TypeId]

  def AnyT: Type.Nominal[TypeId]

  def AnyValT: Type.Nominal[TypeId]

  def BooleanT: Type.Nominal[TypeId]

  def ByteT: Type.Nominal[TypeId]

  def CharT: Type.Nominal[TypeId]

  def ConsT: TypeConstructor[TypeId]

  def DoubleT: Type.Nominal[TypeId]

  def FloatT: Type.Nominal[TypeId]

  def IntT: Type.Nominal[TypeId]

  def ListT: TypeConstructor[TypeId]

  def LongT: Type.Nominal[TypeId]

  def MapT: TypeConstructor[TypeId]

  def NilT: Type.Applied[TypeId]

  def NoneT: Type.Applied[TypeId]

  def NothingT: Type.Nominal[TypeId]

  def NullT: Type.Nominal[TypeId]

  def OptionT: TypeConstructor[TypeId]

  def SetT: TypeConstructor[TypeId]

  def ShortT: Type.Nominal[TypeId]

  def SomeT: TypeConstructor[TypeId]

  def StringT: Type.Nominal[TypeId]

  def UnitT: Type.Nominal[TypeId]

  def VectorT: TypeConstructor[TypeId]
}
