package software.kes.scaletta.internal.runtime

case class UserFunctionSignature(varSpace: VarSpaceSignature,
                                 returnType: Byte,
                                 parameterCount: Int)
