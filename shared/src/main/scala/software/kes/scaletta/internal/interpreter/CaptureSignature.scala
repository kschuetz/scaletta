package software.kes.scaletta.internal.interpreter

trait CaptureSignature {
  def objectCount: Int

  def booleanCount: Int

  def intCount: Int

  def longCount: Int

  def shortCount: Int

  def byteCount: Int

  def charCount: Int

  def doubleCount: Int

  def floatCount: Int

  def isEmpty: Boolean
}

object CaptureSignature {
  def empty: CaptureSignature = Empty

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
      Empty
    } else new Concrete(objectCount, booleanCount, intCount, longCount, shortCount, byteCount, charCount,
      doubleCount, floatCount)
  }

  object Empty extends CaptureSignature {
    def objectCount: Int = 0

    def booleanCount: Int = 0

    def intCount: Int = 0

    def longCount: Int = 0

    def shortCount: Int = 0

    def byteCount: Int = 0

    def charCount: Int = 0

    def doubleCount: Int = 0

    def floatCount: Int = 0

    def isEmpty: Boolean = true

    override def toString: String =
      renderString(this)
  }

  final class Concrete private[CaptureSignature](val objectCount: Int,
                                                 val booleanCount: Int,
                                                 val intCount: Int,
                                                 val longCount: Int,
                                                 val shortCount: Int,
                                                 val byteCount: Int,
                                                 val charCount: Int,
                                                 val doubleCount: Int,
                                                 val floatCount: Int) extends CaptureSignature {

    def isEmpty: Boolean = false

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
      renderString(this)
  }

  private def renderString(sig: CaptureSignature): String =
    s"CaptureSignature(object=${sig.objectCount}, boolean=${sig.booleanCount}, int=${sig.intCount}, long=${sig.longCount}, short=${sig.shortCount}, byte=${sig.byteCount}, char=${sig.charCount}, double=${sig.doubleCount}, float=${sig.floatCount})"
}
