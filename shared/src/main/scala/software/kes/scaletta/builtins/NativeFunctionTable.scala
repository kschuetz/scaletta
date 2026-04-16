package software.kes.scaletta.builtins

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

object NativeFunctionTable {
  def builder: Builder = new Builder()

  final class Builder(private val table: ArrayBuffer[NativeFunction] = ArrayBuffer()) {
    def add(definition: NativeFunction): NativeFunctionId = {
      val result = NativeFunctionId(table.size)
      table += definition
      result
    }

    def result(): NativeFunctionTable =
      new NativeFunctionTable(ArraySeq.from(table))
  }
}

final class NativeFunctionTable private(private val table: ArraySeq[NativeFunction]) {
  def get(id: NativeFunctionId): NativeFunction = table(id.value)

  def size: Int = table.size
}
