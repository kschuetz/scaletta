package software.kes.scaletta.internal.interpreter

object CaptureSignature {
  def create(objectCount: Int,
             booleanCount: Int,
             intCount: Int,
             longCount: Int,
             shortCount: Int,
             byteCount: Int,
             charCount: Int,
             doubleCount: Int,
             floatCount: Int): CaptureSignature = {
    if (objectCount <= 0 && booleanCount <= 0 && intCount <= 0 && longCount <= 0 && shortCount <= 0 &&
      byteCount <= 0 && charCount <= 0 && doubleCount <= 0 && floatCount <= 0) {
      empty
    } else new CaptureSignature(objectCount, booleanCount, intCount, longCount, shortCount, byteCount, charCount,
      doubleCount, floatCount)
  }

  val empty: CaptureSignature = new CaptureSignature(0, 0, 0, 0, 0, 0, 0, 0, 0)
}

final class CaptureSignature private(val objectCount: Int,
                                     val booleanCount: Int,
                                     val intCount: Int,
                                     val longCount: Int,
                                     val shortCount: Int,
                                     val byteCount: Int,
                                     val charCount: Int,
                                     val doubleCount: Int,
                                     val floatCount: Int) {

  override def equals(other: Any): Boolean = other match {
    case that: CaptureSignature =>
      objectCount == that.objectCount &&
        booleanCount == that.booleanCount &&
        intCount == that.intCount &&
        longCount == that.longCount &&
        shortCount == that.shortCount &&
        byteCount == that.byteCount &&
        charCount == that.charCount &&
        doubleCount == that.doubleCount &&
        floatCount == that.floatCount
    case _ => false
  }

  override def hashCode(): Int = {
    var result = objectCount
    result = 31 * result + booleanCount
    result = 31 * result + intCount
    result = 31 * result + longCount
    result = 31 * result + shortCount
    result = 31 * result + byteCount
    result = 31 * result + charCount
    result = 31 * result + doubleCount
    result = 31 * result + floatCount
    result
  }

  override def toString: String =
    s"CaptureSignature(obj=$objectCount, bool=$booleanCount, int=$intCount, long=$longCount, short=$shortCount, byte=$byteCount, char=$charCount, double=$doubleCount, float=$floatCount)"
}
