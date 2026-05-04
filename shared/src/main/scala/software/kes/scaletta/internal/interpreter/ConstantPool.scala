package software.kes.scaletta.internal.interpreter

import scala.collection.immutable.ArraySeq

final class ConstantPool(val longs: ArraySeq[Long],
                         val doubles: ArraySeq[Double],
                         val floats: ArraySeq[Float],
                         val objects: ArraySeq[Any]) {
  def getObject(index: Int): Any = objects(index)

  def getLong(index: Int): Long = longs(index)

  def getDouble(index: Int): Double = doubles(index)

  def getFloat(index: Int): Float = floats(index)
}
