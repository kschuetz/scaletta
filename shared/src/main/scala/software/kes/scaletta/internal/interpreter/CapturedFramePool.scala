package software.kes.scaletta.internal.interpreter

import scala.collection.mutable.ArrayBuffer

private[interpreter] final class CapturedFramePool(maxRetained: Int) {
  private val free = new ArrayBuffer[CapturedFrame]()
  private val borrowed = new ArrayBuffer[CapturedFrame]()

  def acquire(signature: CaptureSignature): CapturedFrame = {
    var found: CapturedFrame = null
    var i = free.length - 1
    while (i >= 0 && (found eq null)) {
      val f = free(i)
      if (f.signature == signature) {
        found = f
        free.remove(i)
      }
      i -= 1
    }

    val frame = if (found ne null) found else CapturedFrame.create(signature)
    borrowed += frame
    frame
  }

  def endRun(): Unit = {
    var i = 0
    while (i < borrowed.length) {
      val frame = borrowed(i)
      frame.clearObjects()
      if (free.length < maxRetained) {
        free += frame
      }
      i += 1
    }
    borrowed.clear()
  }

  def clear(): Unit = {
    free.clear()
    borrowed.clear()
  }
}
