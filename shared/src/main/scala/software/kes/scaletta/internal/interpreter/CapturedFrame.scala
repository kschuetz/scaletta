package software.kes.scaletta.internal.interpreter

object CapturedFrame {
  def create(signature: CaptureSignature): CapturedFrame =
    if (signature.isEmpty) empty else new CapturedFrame(signature)

  val empty: CapturedFrame = new CapturedFrame(CaptureSignature.empty)
}

final class CapturedFrame private(private var _signature: CaptureSignature) {
  private[interpreter] var objects: Array[AnyRef] = if (_signature.objectCount > 0) new Array[AnyRef](_signature.objectCount) else null
  private[interpreter] var booleans: Array[Boolean] = if (_signature.booleanCount > 0) new Array[Boolean](_signature.booleanCount) else null
  private[interpreter] var ints: Array[Int] = if (_signature.intCount > 0) new Array[Int](_signature.intCount) else null
  private[interpreter] var longs: Array[Long] = if (_signature.longCount > 0) new Array[Long](_signature.longCount) else null
  private[interpreter] var shorts: Array[Short] = if (_signature.shortCount > 0) new Array[Short](_signature.shortCount) else null
  private[interpreter] var bytes: Array[Byte] = if (_signature.byteCount > 0) new Array[Byte](_signature.byteCount) else null
  private[interpreter] var chars: Array[Char] = if (_signature.charCount > 0) new Array[Char](_signature.charCount) else null
  private[interpreter] var doubles: Array[Double] = if (_signature.doubleCount > 0) new Array[Double](_signature.doubleCount) else null
  private[interpreter] var floats: Array[Float] = if (_signature.floatCount > 0) new Array[Float](_signature.floatCount) else null

  def signature: CaptureSignature = _signature

  def isEmpty: Boolean = _signature.isEmpty

  def clearObjects(): Unit = {
    if (objects != null) {
      java.util.Arrays.fill(objects, null)
    }
  }

  private[interpreter] def setSignature(sig: CaptureSignature): Unit = {
    if (this eq CapturedFrame.empty) {
      throw new IllegalStateException("Cannot morph the empty CapturedFrame")
    }
    _signature = sig
    if (sig.objectCount > (if (objects == null) 0 else objects.length)) objects = new Array[AnyRef](sig.objectCount)
    if (sig.booleanCount > (if (booleans == null) 0 else booleans.length)) booleans = new Array[Boolean](sig.booleanCount)
    if (sig.intCount > (if (ints == null) 0 else ints.length)) ints = new Array[Int](sig.intCount)
    if (sig.longCount > (if (longs == null) 0 else longs.length)) longs = new Array[Long](sig.longCount)
    if (sig.shortCount > (if (shorts == null) 0 else shorts.length)) shorts = new Array[Short](sig.shortCount)
    if (sig.byteCount > (if (bytes == null) 0 else bytes.length)) bytes = new Array[Byte](sig.byteCount)
    if (sig.charCount > (if (chars == null) 0 else chars.length)) chars = new Array[Char](sig.charCount)
    if (sig.doubleCount > (if (doubles == null) 0 else doubles.length)) doubles = new Array[Double](sig.doubleCount)
    if (sig.floatCount > (if (floats == null) 0 else floats.length)) floats = new Array[Float](sig.floatCount)
  }
}
