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

  object int {
    object eq {
      val int: NativeFunctionId = resolve(CoreTypes.IntT, EqualityOps.eq.name, SignatureQuery.int)
    }
  }

  private def resolve(typ: Type.Nominal[TypeId],
                      name: Name,
                      signatureQuery: SignatureQuery): NativeFunctionId =
    methodResolver.resolveBestMethod(typ, name, signatureQuery)
      .getOrElse(throw new AssertionError)
      .nativeFunctionId
}
