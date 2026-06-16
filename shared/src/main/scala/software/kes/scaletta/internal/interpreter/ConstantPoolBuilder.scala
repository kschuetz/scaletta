package software.kes.scaletta.internal.interpreter

import scala.collection.immutable.ArraySeq
import scala.collection.mutable

object ConstantPoolBuilder {
  def create(): ConstantPoolBuilder = new ConstantPoolBuilder
}

final class ConstantPoolBuilder private() extends ConstantInterner {
  private val objectPool = mutable.ArrayBuffer[AnyRef](null)
  private val stringIndices = mutable.Map.empty[String, Int]
  private val longPool = mutable.ArrayBuffer.empty[Long]
  private val longIndices = mutable.Map.empty[Long, Int]
  private val doublePool = mutable.ArrayBuffer.empty[Double]
  private val doubleIndices = mutable.Map.empty[Double, Int]
  private val floatPool = mutable.ArrayBuffer.empty[Float]
  private val floatIndices = mutable.Map.empty[Float, Int]

  def internObject(value: AnyRef): Int = {
    if (value == null) {
      0
    } else {
      value match {
        case s: String =>
          stringIndices.get(s) match {
            case Some(index) => index
            case None =>
              val index = objectPool.size
              objectPool += s
              stringIndices += (s -> index)
              index
          }
        case _ =>
          val index = objectPool.size
          objectPool += value
          index
      }
    }
  }

  def internLong(value: Long): Int = {
    longIndices.get(value) match {
      case Some(index) => index
      case None =>
        val index = longPool.size
        longPool += value
        longIndices += (value -> index)
        index
    }
  }

  def internDouble(value: Double): Int = {
    doubleIndices.get(value) match {
      case Some(index) => index
      case None =>
        val index = doublePool.size
        doublePool += value
        doubleIndices += (value -> index)
        index
    }
  }

  def internFloat(value: Float): Int = {
    floatIndices.get(value) match {
      case Some(index) => index
      case None =>
        val index = floatPool.size
        floatPool += value
        floatIndices += (value -> index)
        index
    }
  }

  def build(): ConstantPool = {
    new ConstantPool(
      ArraySeq.from(longPool),
      ArraySeq.from(doublePool),
      ArraySeq.from(floatPool),
      ArraySeq.from(objectPool)
    )
  }
}
