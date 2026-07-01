package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api._
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.types._

private[scaletta] final class StandardTypesImpl(registry: TypeRegistryBootstrap)
  extends StandardTypes {

  import StandardTypes.names
  import software.kes.scaletta.api.Packages

  val AnyT: Type[TypeId] = {
    registry.addAlias(base(names.AnyT), CoreTypes.AnyT)
    CoreTypes.AnyT
  }

  val AnyValT: Type[TypeId] = {
    registry.addAlias(base(names.AnyValT), CoreTypes.AnyValT)
    CoreTypes.AnyValT
  }

  val AnyRefT: Type[TypeId] = {
    registry.addAlias(base(names.AnyRefT), CoreTypes.AnyRefT)
    CoreTypes.AnyRefT
  }

  val NullT: Type[TypeId] = {
    registry.addAlias(base(names.NullT), CoreTypes.NullT)
    CoreTypes.NullT
  }

  val NothingT: Type[TypeId] = {
    registry.addAlias(base(names.NothingT), CoreTypes.NothingT)
    CoreTypes.NothingT
  }

  val UnitT: Type[TypeId] = {
    registry.addAlias(base(names.UnitT), CoreTypes.UnitT)
    CoreTypes.UnitT
  }

  val BooleanT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.BooleanT), CoreTypes.BooleanT)

  val ByteT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.ByteT), CoreTypes.ByteT)

  val CharT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.CharT), CoreTypes.CharT)

  val DoubleT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.DoubleT), CoreTypes.DoubleT)

  val FloatT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.FloatT), CoreTypes.FloatT)

  val IntT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.IntT), CoreTypes.IntT)

  val LongT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.LongT), CoreTypes.LongT)

  val ShortT: Type.Nominal[TypeId] =
    registry.registerCoreValueType(base(names.ShortT), CoreTypes.ShortT)

  val StringT: Type.Nominal[TypeId] =
    registry.registerCoreRefType(base(names.StringT), CoreTypes.StringT)

  val OptionT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(base(names.OptionT), TypeParameter.covariant)

  val SomeT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(base(names.SomeT), TypeParameter.covariant)

  val NoneT: Type.Applied[TypeId] =
    TypeApplier.fromNode(OptionT).applyAll(NothingT).asInstanceOf[Type.Applied[TypeId]]

  val VectorT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(collection(names.VectorT), TypeParameter.covariant)

  val SetT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(collection(names.SetT), TypeParameter.covariant)

  val ListT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(collection(names.ListT), TypeParameter.covariant)

  val ConsT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(collection(names.ConsT), TypeParameter.covariant)

  val NilT: Type.Applied[TypeId] =
    TypeApplier.fromNode(ListT).applyAll(NothingT).asInstanceOf[Type.Applied[TypeId]]

  val MapT: Type.Constructor[TypeId] =
    registry.addTypeConstructor(collection(names.MapT), TypeParameter.invariant, TypeParameter.covariant)

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
