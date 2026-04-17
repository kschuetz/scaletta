package software.kes.scaletta.interpreter

import software.kes.scaletta.runtime.{FrameSignature, VarSpaceSignature}

import scala.collection.immutable.ArraySeq

case class UserFunction(varSpaceSignature: VarSpaceSignature,
                        frameSignature: FrameSignature,
                        instructions: ArraySeq[Int]) {
  def fetch(address: Int): Int =
    instructions(address)
}
