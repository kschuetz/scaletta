package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.internal.runtime.VarSpaceSignature
import software.kes.scaletta.util.NonEmptyVector

import scala.collection.mutable

object ProgramBuilder {
  def create(returnType: Byte, mainSignature: VarSpaceSignature): ProgramBuilder =
    create(returnType, mainSignature, 0)

  /**
   * Creates a new ProgramBuilder.
   *
   * @param returnType    The return type of the program (from BasicTypes).
   * @param mainSignature The variable space signature for the main function.
   * @param parameterCount The number of parameters for the main function.
   * @return A new ProgramBuilder instance.
   */
  def create(returnType: Byte, mainSignature: VarSpaceSignature, parameterCount: Int): ProgramBuilder = {
    val constantPoolBuilder = ConstantPoolBuilder.create()
    val mainFunction = UserFunctionBuilder.create(mainSignature, parameterCount)
    new ProgramBuilder(returnType, constantPoolBuilder, mainFunction)
  }
}

final class ProgramBuilder private(private val returnType: Byte,
                                   private val constantPoolBuilder: ConstantPoolBuilder,
                                   private val mainFunctionBuilder: UserFunctionBuilder) {

  private val additionalFunctions = mutable.ArrayBuffer.empty[UserFunctionBuilder]

  /**
   * Returns an Assembler for the main function.
   */
  def mainAssembler(): Assembler = new Assembler(mainFunctionBuilder, constantPoolBuilder)

  /**
   * Adds a new function to the program and returns an Assembler for it.
   *
   * @param signature The variable space signature for the new function.
   * @return An Assembler for the newly created function.
   */
  def addFunction(signature: VarSpaceSignature): Assembler =
    addFunction(signature, 0)

  /**
   * Adds a new function to the program and returns an Assembler for it.
   *
   * @param signature      The variable space signature for the new function.
   * @param parameterCount The number of parameters for the new function.
   * @return An Assembler for the newly created function.
   */
  def addFunction(signature: VarSpaceSignature, parameterCount: Int): Assembler = {
    val builder = UserFunctionBuilder.create(signature, parameterCount)
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
    Program(constantPoolBuilder.build(), functions, returnType)
  }
}
