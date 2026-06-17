package software.kes.scaletta.internal.runtime

import software.kes.scaletta.common.BasicType

case class UserFunctionSignature(varSpace: VarSpaceSignature,
                                 returnType: BasicType,
                                 parameterCount: Int)
