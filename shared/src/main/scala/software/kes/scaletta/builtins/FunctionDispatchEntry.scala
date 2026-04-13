package software.kes.scaletta.builtins

import software.kes.scaletta.runtime.ParamsSignature

case class FunctionDispatchEntry(params: ParamsSignature,
                                 returnType: Int,
                                 impl: FunctionImpl)
