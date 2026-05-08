package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api._
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.types._

private[scaletta] final class StandardTypesImpl(registry: TypeRegistryBootstrap)
  extends StandardTypes {

  import StandardTypes.names
  import software.kes.scaletta.api.Packages

  val AnyT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.AnyT), CoreTypes.AnyT)

  val AnyValT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.AnyValT), CoreTypes.AnyValT)

  val AnyRefT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.AnyRefT), CoreTypes.AnyRefT)

  val NullT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.NullT), CoreTypes.NullT)

  val NothingT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.NothingT), CoreTypes.NothingT)

  val UnitT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.UnitT), CoreTypes.UnitT)

  val BooleanT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.BooleanT), CoreTypes.BooleanT)

  val ByteT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.ByteT), CoreTypes.ByteT)

  val CharT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.CharT), CoreTypes.CharT)

  val DoubleT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.DoubleT), CoreTypes.DoubleT)

  val FloatT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.FloatT), CoreTypes.FloatT)

  val IntT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.IntT), CoreTypes.IntT)

  val LongT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.LongT), CoreTypes.LongT)

  val ShortT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.ShortT), CoreTypes.ShortT)

  val StringT: Type.Nominal[TypeId] =
    registry.registerCore(base(names.StringT), CoreTypes.StringT)

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

  registry.addRelationship(SomeT, OptionT.applyAll(Type.variable(0)))
  registry.addRelationship(ConsT, ListT.applyAll(Type.variable(0)))

  private def base(name: Name): QualifiedName.Full =
    Packages.scaletta.qualify(name)

  private def collection(name: Name): QualifiedName.Full =
    Packages.scalettaCollection.qualify(name)
}
