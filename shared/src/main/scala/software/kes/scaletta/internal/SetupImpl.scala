package software.kes.scaletta.internal

import software.kes.scaletta.api._

private[scaletta] final class SetupImpl(
                                         val methodRegistry: MethodRegistry,
                                         val typeRegistry: TypeRegistry,
                                         val runtimeContextRegistry: RuntimeContextRegistry
                                       ) extends Setup
