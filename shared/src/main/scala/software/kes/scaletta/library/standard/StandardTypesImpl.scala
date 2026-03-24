package software.kes.scaletta.library.standard

import software.kes.scaletta.symbols.{Name, QualifiedName}
import software.kes.scaletta.types._

private[scaletta] final class StandardTypesImpl(registry: TypeRegistryBootstrap)
  extends StandardTypes {

  import StandardTypes.names
  import software.kes.scaletta.common.Packages

  val AnyT: Type.Nominal[TypeId] =
    registry.addTop(fqn(names.AnyT))

  val AnyValT: Type.Nominal[TypeId] =
    registry.addTopValue(fqn(names.AnyValT))

  val AnyRefT: Type.Nominal[TypeId] =
    registry.addTopRef(fqn(names.AnyRefT))

  val NothingT: Type.Nominal[TypeId] =
    registry.addBottom(fqn(names.NothingT))

  val NullT: Type.Nominal[TypeId] =
    registry.addBottomRef(fqn(names.NullT))

  val UnitT: Type.Nominal[TypeId] =
    registry.addValueType(fqn(names.UnitT))

  val BooleanT: Type.Nominal[TypeId] =
    registry.addValueType(fqn(names.BooleanT))

  val ByteT: Type.Nominal[TypeId] =
    registry.addValueType(fqn(names.ByteT))

  val ShortT: Type.Nominal[TypeId] =
    registry.addValueType(fqn(names.ShortT))

  val IntT: Type.Nominal[TypeId] =
    registry.addValueType(fqn(names.IntT))

  val LongT: Type.Nominal[TypeId] =
    registry.addValueType(fqn(names.LongT))

  val FloatT: Type.Nominal[TypeId] =
    registry.addValueType(fqn(names.FloatT))

  val DoubleT: Type.Nominal[TypeId] =
    registry.addValueType(fqn(names.DoubleT))

  val CharT: Type.Nominal[TypeId] =
    registry.addValueType(fqn(names.CharT))

  val StringT: Type.Nominal[TypeId] =
    registry.addRefType(fqn(names.StringT))

  val OptionT: TypeConstructor[TypeId] =
    registry.addTypeConstructor(fqn(names.OptionT), TypeParameter.covariant)

  val SomeT: TypeConstructor[TypeId] =
    registry.addTypeConstructor(fqn(names.SomeT), TypeParameter.covariant)

  val NoneT: Type.Nominal[TypeId] =
    registry.addRefType(fqn(names.NoneT))

  registry.addRelationship(OptionT, SomeT)

  private def fqn(name: Name): QualifiedName.Full = {
    Packages.scaletta.qualify(name)
  }
}
