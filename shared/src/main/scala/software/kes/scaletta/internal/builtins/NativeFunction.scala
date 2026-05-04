package software.kes.scaletta.internal.builtins

import software.kes.scaletta.runtime.ParamsSignature

case class NativeFunction(params: ParamsSignature,
                          returnType: Int,
                          impl: FunctionImpl)
