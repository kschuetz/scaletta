package software.kes.scaletta.internal.library.standard.testsupport

import software.kes.scaletta.api.{Name, NativeFunctionId, Type, TypeId}
import software.kes.scaletta.internal.builtins.MethodResolver
import software.kes.scaletta.internal.library.standard.ComparisonOps
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.symbols.SignatureQuery

final class ComparisonOpsLookup(methodResolver: MethodResolver) {
  abstract class MethodBase1(typ: Type.Nominal[TypeId], name: Name) {
    final protected def resolve(rhs: Type[TypeId]): NativeFunctionId =
      methodResolver.resolveBestMethod(typ, name, SignatureQuery.of(rhs))
        .getOrElse(throw new AssertionError(s"Could not resolve method $name for types $typ and $rhs"))
        .nativeFunctionId
  }

  abstract class MethodBase2(typ: Type.Nominal[TypeId], name: Name) extends MethodBase1(typ, name) {
    val byte: NativeFunctionId = resolve(CoreTypes.ByteT)
    val char: NativeFunctionId = resolve(CoreTypes.CharT)
    val double: NativeFunctionId = resolve(CoreTypes.DoubleT)
    val float: NativeFunctionId = resolve(CoreTypes.FloatT)
    val int: NativeFunctionId = resolve(CoreTypes.IntT)
    val long: NativeFunctionId = resolve(CoreTypes.LongT)
    val short: NativeFunctionId = resolve(CoreTypes.ShortT)
  }

  abstract class TypeBase(typ: Type.Nominal[TypeId]) {
    object lt extends MethodBase2(typ, ComparisonOps.lt.name)

    object gt extends MethodBase2(typ, ComparisonOps.gt.name)

    object le extends MethodBase2(typ, ComparisonOps.le.name)

    object ge extends MethodBase2(typ, ComparisonOps.ge.name)
  }

  abstract class BooleanBase(name: Name) extends MethodBase1(CoreTypes.BooleanT, name) {
    val boolean: NativeFunctionId = resolve(CoreTypes.BooleanT)
  }

  abstract class StringBase(name: Name) extends MethodBase1(CoreTypes.StringT, name) {
    val string: NativeFunctionId = resolve(CoreTypes.StringT)
  }

  object boolean {
    object lt extends BooleanBase(ComparisonOps.lt.name)

    object gt extends BooleanBase(ComparisonOps.gt.name)

    object le extends BooleanBase(ComparisonOps.le.name)

    object ge extends BooleanBase(ComparisonOps.ge.name)
  }

  object byte extends TypeBase(CoreTypes.ByteT)

  object char extends TypeBase(CoreTypes.CharT)

  object double extends TypeBase(CoreTypes.DoubleT)

  object float extends TypeBase(CoreTypes.FloatT)

  object int extends TypeBase(CoreTypes.IntT)

  object long extends TypeBase(CoreTypes.LongT)

  object short extends TypeBase(CoreTypes.ShortT)

  object string {
    object lt extends StringBase(ComparisonOps.lt.name)

    object gt extends StringBase(ComparisonOps.gt.name)

    object le extends StringBase(ComparisonOps.le.name)

    object ge extends StringBase(ComparisonOps.ge.name)
  }
}
