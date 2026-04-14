package software.kes.scaletta.interpreter

import software.kes.scaletta.runtime.VarSpaceSignature
import software.kes.scaletta.util.NonEmptyVector

import scala.collection.immutable.ArraySeq

case class Program(constantPool: ConstantPool,
                   functions: NonEmptyVector[LocalFunction],
                   returnType: Byte)

case class LocalFunction(signature: VarSpaceSignature,
                         instructions: ArraySeq[Int])
