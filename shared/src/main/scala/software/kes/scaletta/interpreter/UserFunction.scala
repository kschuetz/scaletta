package software.kes.scaletta.interpreter

import software.kes.scaletta.runtime.{FrameSignature, VarSpaceSignature}

import scala.collection.immutable.ArraySeq

case class UserFunction(varSpaceSignature: VarSpaceSignature,
                        instructions: ArraySeq[Int]) {
  def frameSignature: FrameSignature = varSpaceSignature.frameSignature

  def fetch(address: Int): Int =
    instructions(address)
}
