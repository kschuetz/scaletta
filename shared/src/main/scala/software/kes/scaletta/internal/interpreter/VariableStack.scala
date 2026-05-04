package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.runtime.FrameSignature
import software.kes.scaletta.util.stack._

object VariableStack {
  def create(): VariableStack =
    new VariableStack(ObjectStack.create(), BooleanStack.create(), IntStack.create(),
      LongStack.create(), ShortStack.create(), ByteStack.create(), CharStack.create(),
      DoubleStack.create(), FloatStack.create())
}

final class VariableStack(private[interpreter] val objects: ObjectStack,
                          private[interpreter] val booleans: BooleanStack,
                          private[interpreter] val ints: IntStack,
                          private[interpreter] val longs: LongStack,
                          private[interpreter] val shorts: ShortStack,
                          private[interpreter] val bytes: ByteStack,
                          private[interpreter] val chars: CharStack,
                          private[interpreter] val doubles: DoubleStack,
                          private[interpreter] val floats: FloatStack
                         ) {

  def clear(): Unit = {
    objects.clear()
    booleans.clear()
    ints.clear()
    longs.clear()
    shorts.clear()
    bytes.clear()
    chars.clear()
    doubles.clear()
    floats.clear()
  }

  def expandFrame(signature: FrameSignature): Unit = {
    objects.expand(signature.objectCount)
    booleans.expand(signature.booleanCount)
    ints.expand(signature.intCount)
    longs.expand(signature.longCount)
    shorts.expand(signature.shortCount)
    bytes.expand(signature.byteCount)
    chars.expand(signature.charCount)
    doubles.expand(signature.doubleCount)
    floats.expand(signature.floatCount)
  }

  def contractFrame(signature: FrameSignature): Unit = {
    objects.contract(signature.objectCount)
    booleans.contract(signature.booleanCount)
    ints.contract(signature.intCount)
    longs.contract(signature.longCount)
    shorts.contract(signature.shortCount)
    bytes.contract(signature.byteCount)
    chars.contract(signature.charCount)
    doubles.contract(signature.doubleCount)
    floats.contract(signature.floatCount)
  }
}
