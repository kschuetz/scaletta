package software.kes.scaletta.builtins

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ArrayBuffer

object FunctionDispatchTable {
  def builder: Builder = new Builder()

  final class Builder(private val table: ArrayBuffer[FunctionDispatchEntry] = ArrayBuffer()) {
    def add(definition: FunctionDispatchEntry): FunctionId = {
      val result = FunctionId(table.size)
      table += definition
      result
    }

    def result(): FunctionDispatchTable =
      new FunctionDispatchTable(ArraySeq.from(table))
  }
}

final class FunctionDispatchTable private(private val table: ArraySeq[FunctionDispatchEntry]) {
  def get(id: FunctionId): FunctionDispatchEntry = table(id.value)

  def size: Int = table.size
}
