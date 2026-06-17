package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.util.NonEmptyVector

case class Program(constantPool: ConstantPool,
                   functions: NonEmptyVector[UserFunction]) {
  def mainFunction: UserFunction = functions.head

  def returnType: Byte = mainFunction.returnType
}
