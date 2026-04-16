package software.kes.scaletta.interpreter

import software.kes.scaletta.runtime.{FrameSignature, VarSpaceSignature}
import software.kes.scaletta.util.NonEmptyVector

import scala.collection.immutable.ArraySeq

case class Program(constantPool: ConstantPool,
                   functions: NonEmptyVector[UserFunction],
                   returnType: Byte) {
  def mainFunction: UserFunction = functions.head
}

case class UserFunction(varSpaceSignature: VarSpaceSignature,
                        frameSignature: FrameSignature,
                        instructions: ArraySeq[Int]) {
  def getInstructionOrOperand(address: Int): Int =
    instructions(address)
}
