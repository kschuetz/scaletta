package software.kes.scaletta.internal.library.standard.testsupport

import software.kes.scaletta.api.{Name, NativeFunctionId, Type, TypeId}
import software.kes.scaletta.internal.builtins.MethodResolver
import software.kes.scaletta.internal.library.standard.ComparisonOps
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.symbols.SignatureQuery

final class ComparisonOpsLookup(methodResolver: MethodResolver) {
  abstract class MethodBase(typ: Type.Nominal[TypeId], name: Name) {
    val boolean: NativeFunctionId = resolve(CoreTypes.BooleanT)
    val byte: NativeFunctionId = resolve(CoreTypes.ByteT)
    val char: NativeFunctionId = resolve(CoreTypes.CharT)
    val double: NativeFunctionId = resolve(CoreTypes.DoubleT)
    val float: NativeFunctionId = resolve(CoreTypes.FloatT)
    val int: NativeFunctionId = resolve(CoreTypes.IntT)
    val long: NativeFunctionId = resolve(CoreTypes.LongT)
    val short: NativeFunctionId = resolve(CoreTypes.ShortT)
    val string: NativeFunctionId = resolve(CoreTypes.StringT)

    private def resolve(rhs: Type[TypeId]): NativeFunctionId =
      methodResolver.resolveBestMethod(typ, name, SignatureQuery.of(rhs))
        .getOrElse(throw new AssertionError(s"Could not resolve method $name for types $typ and $rhs"))
        .nativeFunctionId
  }

  abstract class TypeBase(typ: Type.Nominal[TypeId]) {
    object lt extends MethodBase(typ, ComparisonOps.lt.name)

    object gt extends MethodBase(typ, ComparisonOps.gt.name)

    object le extends MethodBase(typ, ComparisonOps.le.name)

    object ge extends MethodBase(typ, ComparisonOps.ge.name)
  }

  object boolean extends TypeBase(CoreTypes.BooleanT)

  object byte extends TypeBase(CoreTypes.ByteT)

  object char extends TypeBase(CoreTypes.CharT)

  object double extends TypeBase(CoreTypes.DoubleT)

  object float extends TypeBase(CoreTypes.FloatT)

  object int extends TypeBase(CoreTypes.IntT)

  object long extends TypeBase(CoreTypes.LongT)

  object short extends TypeBase(CoreTypes.ShortT)

  object string extends TypeBase(CoreTypes.StringT)
}
