package software.kes.scaletta.library.standard

import software.kes.scaletta.symbols.{Name, QualifiedName}
import software.kes.scaletta.types._

private[scaletta] final class StandardTypesImpl(registry: TypeRegistryBootstrap)
  extends StandardTypes {

  import StandardTypes.names
  import software.kes.scaletta.common.Packages

  val AnyT: Type.Nominal[TypeId] =
    registry.addTop(base(names.AnyT))

  val AnyValT: Type.Nominal[TypeId] =
    registry.addTopValue(base(names.AnyValT))

  val AnyRefT: Type.Nominal[TypeId] =
    registry.addTopRef(base(names.AnyRefT))

  val NothingT: Type.Nominal[TypeId] =
    registry.addBottom(base(names.NothingT))

  val NullT: Type.Nominal[TypeId] =
    registry.addBottomRef(base(names.NullT))

  val UnitT: Type.Nominal[TypeId] =
    registry.addValueType(base(names.UnitT))

  val BooleanT: Type.Nominal[TypeId] =
    registry.addValueType(base(names.BooleanT))

  val ByteT: Type.Nominal[TypeId] =
    registry.addValueType(base(names.ByteT))

  val ShortT: Type.Nominal[TypeId] =
    registry.addValueType(base(names.ShortT))

  val IntT: Type.Nominal[TypeId] =
    registry.addValueType(base(names.IntT))

  val LongT: Type.Nominal[TypeId] =
    registry.addValueType(base(names.LongT))

  val FloatT: Type.Nominal[TypeId] =
    registry.addValueType(base(names.FloatT))

  val DoubleT: Type.Nominal[TypeId] =
    registry.addValueType(base(names.DoubleT))

  val CharT: Type.Nominal[TypeId] =
    registry.addValueType(base(names.CharT))

  val StringT: Type.Nominal[TypeId] =
    registry.addRefType(base(names.StringT))

  val OptionT: TypeConstructor[TypeId] =
    registry.addTypeConstructor(base(names.OptionT), TypeParameter.covariant)

  val SomeT: TypeConstructor[TypeId] =
    registry.addTypeConstructor(base(names.SomeT), TypeParameter.covariant)

  val NoneT: Type.Applied[TypeId] =
    OptionT.applyAll(NothingT)

  val VectorT: TypeConstructor[TypeId] =
    registry.addTypeConstructor(collection(names.VectorT), TypeParameter.covariant)

  val SetT: TypeConstructor[TypeId] =
    registry.addTypeConstructor(collection(names.SetT), TypeParameter.covariant)

  val ListT: TypeConstructor[TypeId] =
    registry.addTypeConstructor(collection(names.ListT), TypeParameter.covariant)

  val ConsT: TypeConstructor[TypeId] =
    registry.addTypeConstructor(collection(names.ConsT), TypeParameter.covariant)

  val NilT: Type.Applied[TypeId] =
    ListT.applyAll(NothingT)

  val MapT: TypeConstructor[TypeId] =
    registry.addTypeConstructor(collection(names.MapT), TypeParameter.invariant, TypeParameter.covariant)

  registry.addRelationship(OptionT, SomeT)
  registry.addRelationship(ListT, ConsT)

  private def base(name: Name): QualifiedName.Full =
    Packages.scaletta.qualify(name)

  private def collection(name: Name): QualifiedName.Full =
    Packages.scalettaCollection.qualify(name)
}
