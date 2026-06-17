package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.common.BasicType
import software.kes.scaletta.internal.runtime.{FrameSignature, UserFunctionSignature, VarSpaceSignature}

import scala.collection.immutable.ArraySeq

case class UserFunction(signature: UserFunctionSignature,
                        instructions: ArraySeq[Int]) {
  def varSpaceSignature: VarSpaceSignature = signature.varSpace

  def frameSignature: FrameSignature = varSpaceSignature.frameSignature

  def returnType: BasicType = signature.returnType

  def parameterCount: Int = signature.parameterCount

  def fetch(address: Int): Int =
    instructions(address)
}
