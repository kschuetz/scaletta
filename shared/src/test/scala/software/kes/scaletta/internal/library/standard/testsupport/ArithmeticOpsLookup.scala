package software.kes.scaletta.internal.library.standard.testsupport

import software.kes.scaletta.api.{Name, NativeFunctionId, Type, TypeId}
import software.kes.scaletta.internal.builtins.MethodResolver
import software.kes.scaletta.internal.library.standard.ArithmeticOps
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.symbols.SignatureQuery

final class ArithmeticOpsLookup(methodResolver: MethodResolver) {
  abstract class MethodBase(typ: Type.Nominal[TypeId], name: Name) {
    val byte: NativeFunctionId = resolve(CoreTypes.ByteT)
    val char: NativeFunctionId = resolve(CoreTypes.CharT)
    val double: NativeFunctionId = resolve(CoreTypes.DoubleT)
    val float: NativeFunctionId = resolve(CoreTypes.FloatT)
    val int: NativeFunctionId = resolve(CoreTypes.IntT)
    val long: NativeFunctionId = resolve(CoreTypes.LongT)
    val short: NativeFunctionId = resolve(CoreTypes.ShortT)

    private def resolve(rhs: Type[TypeId]): NativeFunctionId =
      methodResolver.resolveBestMethod(typ, name, SignatureQuery.of(rhs))
        .getOrElse(throw new AssertionError)
        .nativeFunctionId
  }

  abstract class TypeBase(typ: Type.Nominal[TypeId]) {
    object add extends MethodBase(typ, ArithmeticOps.add.name)

    object subtract extends MethodBase(typ, ArithmeticOps.subtract.name)

    object multiply extends MethodBase(typ, ArithmeticOps.multiply.name)

    object divide extends MethodBase(typ, ArithmeticOps.divide.name)

    object modulo extends MethodBase(typ, ArithmeticOps.modulo.name)
  }

  object byte extends TypeBase(CoreTypes.ByteT)

  object char extends TypeBase(CoreTypes.CharT)

  object double extends TypeBase(CoreTypes.DoubleT)

  object float extends TypeBase(CoreTypes.FloatT)

  object int extends TypeBase(CoreTypes.IntT)

  object long extends TypeBase(CoreTypes.LongT)

  object short extends TypeBase(CoreTypes.ShortT)
}
