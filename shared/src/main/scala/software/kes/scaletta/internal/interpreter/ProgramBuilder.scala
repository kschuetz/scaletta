package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.internal.runtime.UserFunctionSignature
import software.kes.scaletta.util.NonEmptyVector

import scala.collection.mutable

object ProgramBuilder {
  def create(mainSignature: UserFunctionSignature): ProgramBuilder = {
    val constantPoolBuilder = ConstantPoolBuilder.create()
    val mainFunction = UserFunctionBuilder.create(mainSignature)
    new ProgramBuilder(constantPoolBuilder, mainFunction)
  }
}

final class ProgramBuilder private(private val constantPoolBuilder: ConstantPoolBuilder,
                                   private val mainFunctionBuilder: UserFunctionBuilder) {

  private val additionalFunctions = mutable.ArrayBuffer.empty[UserFunctionBuilder]

  /**
   * Returns an Assembler for the main function.
   */
  def mainAssembler(): Assembler = new Assembler(mainFunctionBuilder, constantPoolBuilder)

  /**
   * Adds a new function to the program and returns an Assembler for it.
   *
   * @param signature The signature for the new function.
   * @return An Assembler for the newly created function.
   */
  def addFunction(signature: UserFunctionSignature): Assembler = {
    val builder = UserFunctionBuilder.create(signature)
    additionalFunctions += builder
    new Assembler(builder, constantPoolBuilder)
  }

  /**
   * Builds the final Program.
   *
   * @return The constructed Program instance.
   */
  def build(): Program = {
    val functions = NonEmptyVector(
      mainFunctionBuilder.build(),
      additionalFunctions.toSeq.map(_.build()): _*
    )
    Program(constantPoolBuilder.build(), functions)
  }
}
