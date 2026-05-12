package software.kes.scaletta.internal

import software.kes.scaletta.api._
import software.kes.scaletta.internal.library.standard.StandardTypes

private[scaletta] final class SetupImpl(val methodRegistry: MethodRegistry,
                                        val typeRegistry: TypeRegistry,
                                        val runtimeContextRegistry: RuntimeContextRegistry,
                                        val standardTypes: StandardTypes,
                                       ) extends Setup
