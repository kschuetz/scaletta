package software.kes.scaletta.interpreter

import software.kes.scaletta.runtime.{FrameSignature, VarSpaceSignature}
import software.kes.scaletta.util.NonEmptyVector

import scala.collection.immutable.ArraySeq

case class Program(constantPool: ConstantPool,
                   functions: NonEmptyVector[LocalFunction],
                   returnType: Byte) {
  def mainFunction: LocalFunction = functions.head
}

case class LocalFunction(varSpaceSignature: VarSpaceSignature,
                         frameSignature: FrameSignature,
                         instructions: ArraySeq[Int])
