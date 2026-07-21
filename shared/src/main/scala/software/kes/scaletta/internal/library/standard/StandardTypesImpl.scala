package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api._
import software.kes.scaletta.internal.library.standard.typeinfo.{CollectionTypeInfo, CoreTypeInfo, OptionTypeInfo}
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.types._
import software.kes.scaletta.util.NonEmptyVector

object StandardTypesImpl {
  private val singleCovariant: NonEmptyVector[TypeParameter[TypeId]] =
    NonEmptyVector(TypeParameter.covariant)
}

private[scaletta] final class StandardTypesImpl(registry: TypeRegistryBootstrap)
  extends StandardTypes {

  import StandardTypes.names
  import StandardTypesImpl._
  import software.kes.scaletta.api.Packages

  val AnyT: ProperType[TypeId] = {
    registry.addAlias(base(names.AnyT), CoreTypes.AnyT)
    CoreTypes.AnyT
  }

  val AnyValT: ProperType[TypeId] = {
    registry.addAlias(base(names.AnyValT), CoreTypes.AnyValT)
    CoreTypes.AnyValT
  }

  val AnyRefT: ProperType[TypeId] = {
    registry.addAlias(base(names.AnyRefT), CoreTypes.AnyRefT)
    CoreTypes.AnyRefT
  }

  val NullT: ProperType[TypeId] = {
    registry.addAlias(base(names.NullT), CoreTypes.NullT)
    CoreTypes.NullT
  }

  val NothingT: ProperType[TypeId] = {
    registry.addAlias(base(names.NothingT), CoreTypes.NothingT)
    CoreTypes.NothingT
  }

  val UnitT: ProperType[TypeId] = {
    registry.addAlias(base(names.UnitT), CoreTypes.UnitT)
    CoreTypes.UnitT
  }

  val BooleanT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.BooleanT), CoreTypes.BooleanT, CoreTypeInfo.BooleanT)

  val ByteT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.ByteT), CoreTypes.ByteT, CoreTypeInfo.ByteT)

  val CharT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.CharT), CoreTypes.CharT, CoreTypeInfo.CharT)

  val DoubleT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.DoubleT), CoreTypes.DoubleT, CoreTypeInfo.DoubleT)

  val FloatT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.FloatT), CoreTypes.FloatT, CoreTypeInfo.FloatT)

  val IntT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.IntT), CoreTypes.IntT, CoreTypeInfo.IntT)

  val LongT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.LongT), CoreTypes.LongT, CoreTypeInfo.LongT)

  val ShortT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.ShortT), CoreTypes.ShortT, CoreTypeInfo.ShortT)

  val StringT: Type.Nominal[TypeId] =
    registry.registerCoreRefType(base(names.StringT), CoreTypes.StringT, CoreTypeInfo.StringT)

  val OptionT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(base(names.OptionT),
      singleCovariant, OptionTypeInfo.OptionT)

  val SomeT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(base(names.SomeT),
      singleCovariant, OptionTypeInfo.SomeT)

  val NoneT: Type.Applied[TypeId] =
    TypeApplier.fromNode(OptionT).applyAll(NothingT).asInstanceOf[Type.Applied[TypeId]]

  val VectorT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(collection(names.VectorT),
      singleCovariant, CollectionTypeInfo.VectorT)

  val SetT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(collection(names.SetT),
      singleCovariant, CollectionTypeInfo.SetT)

  val ListT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(collection(names.ListT),
      singleCovariant, CollectionTypeInfo.ListT)

  val ConsT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(collection(names.ConsT),
      singleCovariant, CollectionTypeInfo.ConsT)

  val NilT: Type.Applied[TypeId] =
    TypeApplier.fromNode(ListT).applyAll(NothingT).asInstanceOf[Type.Applied[TypeId]]

  val MapT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(collection(names.MapT),
      NonEmptyVector(TypeParameter.invariant, TypeParameter.covariant),
      CollectionTypeInfo.MapT)

  registry.addRelationship(
    supertype = TypeApplier.fromNode(OptionT).applyAll(Type.variable(0)),
    subtype = TypeApplier.fromNode(SomeT).applyAll(Type.variable(0))
  )
  registry.addRelationship(
    supertype = TypeApplier.fromNode(ListT).applyAll(Type.variable(0)),
    subtype = TypeApplier.fromNode(ConsT).applyAll(Type.variable(0))
  )

  private def base(name: Name): QualifiedName.Full =
    Packages.scaletta.qualify(name)

  private def collection(name: Name): QualifiedName.Full =
    Packages.scalettaCollection.qualify(name)

}
