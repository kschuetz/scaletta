package software.kes.scaletta.internal.interpreter

private[interpreter] final class CapturedFrame(val signature: CaptureSignature) {
  private[interpreter] val objects: Array[AnyRef] = if (signature.objectCount > 0) new Array[AnyRef](signature.objectCount) else null
  private[interpreter] val booleans: Array[Boolean] = if (signature.booleanCount > 0) new Array[Boolean](signature.booleanCount) else null
  private[interpreter] val ints: Array[Int] = if (signature.intCount > 0) new Array[Int](signature.intCount) else null
  private[interpreter] val longs: Array[Long] = if (signature.longCount > 0) new Array[Long](signature.longCount) else null
  private[interpreter] val shorts: Array[Short] = if (signature.shortCount > 0) new Array[Short](signature.shortCount) else null
  private[interpreter] val bytes: Array[Byte] = if (signature.byteCount > 0) new Array[Byte](signature.byteCount) else null
  private[interpreter] val chars: Array[Char] = if (signature.charCount > 0) new Array[Char](signature.charCount) else null
  private[interpreter] val doubles: Array[Double] = if (signature.doubleCount > 0) new Array[Double](signature.doubleCount) else null
  private[interpreter] val floats: Array[Float] = if (signature.floatCount > 0) new Array[Float](signature.floatCount) else null

  def clearObjects(): Unit = {
    if (objects != null) {
      java.util.Arrays.fill(objects, null)
    }
  }
}
