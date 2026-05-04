package software.kes.scaletta.internal.builtins

import software.kes.scaletta.internal.runtime.ParamsSignature

case class NativeFunction(params: ParamsSignature,
                          returnType: Int,
                          impl: FunctionImpl)
