package software.kes.scaletta.internal.library.standard.testsupport

import software.kes.scaletta.api.Packages.scalettaMath
import software.kes.scaletta.api.{Name, NativeFunctionId}
import software.kes.scaletta.internal.builtins.MethodResolver
import software.kes.scaletta.internal.symbols.SignatureQuery

final class MathLookup(methodResolver: MethodResolver) {
  val sqrt: NativeFunctionId = resolveStatic("sqrt", SignatureQuery.double)

  private def resolveStatic(name: String,
                            signatureQuery: SignatureQuery): NativeFunctionId =
    methodResolver.resolveBestStaticFunction(scalettaMath.qualify(Name(name)),
        signatureQuery)
      .getOrElse(throw new AssertionError)
      .nativeFunctionId
}
