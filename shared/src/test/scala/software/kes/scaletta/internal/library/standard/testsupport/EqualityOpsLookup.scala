package software.kes.scaletta.internal.library.standard.testsupport

import software.kes.scaletta.api.{Name, NativeFunctionId, Type, TypeId}
import software.kes.scaletta.internal.builtins.MethodResolver
import software.kes.scaletta.internal.library.standard.EqualityOps
import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.symbols.SignatureQuery

final class EqualityOpsLookup(methodResolver: MethodResolver) {
  val eq: NativeFunctionId = resolve(CoreTypes.AnyT, EqualityOps.eq.name, SignatureQuery.any)
  val neq: NativeFunctionId = resolve(CoreTypes.AnyT, EqualityOps.neq.name, SignatureQuery.any)
  val refEq: NativeFunctionId = resolve(CoreTypes.AnyRefT, EqualityOps.refEq.name, SignatureQuery.anyRef)

  abstract class MethodBase(typ: Type[TypeId], name: Name) {
    val byte: NativeFunctionId = resolve(typ, name, SignatureQuery.byte)
    val char: NativeFunctionId = resolve(typ, name, SignatureQuery.char)
    val double: NativeFunctionId = resolve(typ, name, SignatureQuery.double)
    val float: NativeFunctionId = resolve(typ, name, SignatureQuery.float)
    val int: NativeFunctionId = resolve(typ, name, SignatureQuery.int)
    val long: NativeFunctionId = resolve(typ, name, SignatureQuery.long)
    val short: NativeFunctionId = resolve(typ, name, SignatureQuery.short)
    val any: NativeFunctionId = resolve(typ, name, SignatureQuery.any)
  }

  abstract class TypeBase(typ: Type[TypeId]) {
    object eq extends MethodBase(typ, EqualityOps.eq.name)

    object neq extends MethodBase(typ, EqualityOps.neq.name)
  }

  object byte extends TypeBase(CoreTypes.ByteT)

  object char extends TypeBase(CoreTypes.CharT)

  object double extends TypeBase(CoreTypes.DoubleT)

  object float extends TypeBase(CoreTypes.FloatT)

  object int extends TypeBase(CoreTypes.IntT)

  object long extends TypeBase(CoreTypes.LongT)

  object short extends TypeBase(CoreTypes.ShortT)

  object string extends TypeBase(CoreTypes.StringT)

  private def resolve(typ: Type[TypeId],
                      name: Name,
                      signatureQuery: SignatureQuery): NativeFunctionId =
    methodResolver.resolveBestMethod(typ, name, signatureQuery)
      .getOrElse(throw new AssertionError)
      .nativeFunctionId
}
