package software.kes.scaletta.internal.builtins

import software.kes.scaletta.api.{FormalParameter, Name, NativeFunctionId, ParameterGroup}
import software.kes.scaletta.common.BasicTypes

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

object NativeFunctionTable {
  def builder(): Builder = new Builder()

  final class Builder private[NativeFunctionTable](private val functions: ArrayBuffer[NativeFunction] = ArrayBuffer(),
                                                   private val definitions: ArrayBuffer[NativeFunctionDefinition] = ArrayBuffer()) {
    def size: Int = functions.size

    def getDefinition(id: NativeFunctionId): NativeFunctionDefinition = definitions(id.value)

    def add(function: NativeFunction,
            createDefinition: NativeFunctionId => NativeFunctionDefinition): NativeFunctionDefinition = {
      val result = NativeFunctionId(functions.size)
      val definition = createDefinition(result)
      functions += function
      definitions += definition
      definition
    }

    def add(function: NativeFunction,
            pure: Boolean = false): NativeFunctionId = {
      val result = NativeFunctionId(functions.size)
      val paramList = (0 until function.params.paramCount)
        .foldLeft(Vector.empty[FormalParameter]) { (acc, i) =>
          val bt = function.params.basicTypeOf(i)
          val typ = BasicTypes.toType(bt)
          acc :+ FormalParameter(Name(s"p$i"), typ)
        }
      val paramGroups = if (paramList.isEmpty) {
        Vector.empty
      } else {
        ParameterGroup.single(paramList: _*)
      }
      val returnType = BasicTypes.toType(function.returnType.toByte)
      val definition = NativeFunctionDefinition(
        paramGroups = paramGroups,
        returnType = returnType,
        pure = pure,
        nativeFunctionId = result,
        requireRuntimeContexts = Set.empty
      )
      functions += function
      definitions += definition
      result
    }

    def result(): NativeFunctionTable =
      new NativeFunctionTable(ArraySeq.from(functions), ArraySeq.from(definitions))
  }
}

final class NativeFunctionTable private(private val functions: ArraySeq[NativeFunction],
                                        private val definitions: ArraySeq[NativeFunctionDefinition]) {
  def get(id: NativeFunctionId): NativeFunction = functions(id.value)

  def getDefinition(id: NativeFunctionId): NativeFunctionDefinition = definitions(id.value)

  def size: Int = functions.size
}
