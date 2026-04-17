package software.kes.scaletta.interpreter

import software.kes.scaletta.util.NonEmptyVector

case class Program(constantPool: ConstantPool,
                   functions: NonEmptyVector[UserFunction],
                   returnType: Byte) {
  def mainFunction: UserFunction = functions.head
}
