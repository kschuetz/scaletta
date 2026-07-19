package software.kes.scaletta.internal.interpreter

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

private[interpreter] final class CapturedFramePool(maxRetained: Int) {
  private val freeBySignature = mutable.HashMap[CaptureSignature, ArrayBuffer[CapturedFrame]]()
  private val genericSpare = new ArrayBuffer[CapturedFrame]()
  private val borrowed = new ArrayBuffer[CapturedFrame]()

  private var _totalFree = 0

  // Caps for signature-bucket retention
  private val MaxArrayCap = 64
  private val MaxGenericSpare = 16

  // Metrics
  private var _hits = 0L
  private var _genericHits = 0L
  private var _allocs = 0L
  private var _evictions = 0L

  def acquire(sig: CaptureSignature): CapturedFrame = synchronized {
    if (sig.isEmpty) return CapturedFrame.empty

    val bucketOpt = freeBySignature.get(sig)
    if (bucketOpt.isDefined && bucketOpt.get.nonEmpty) {
      val bucket = bucketOpt.get
      val frame = bucket.remove(bucket.length - 1)
      _totalFree -= 1
      _hits += 1
      borrowed += frame
      frame
    } else if (genericSpare.nonEmpty) {
      val frame = genericSpare.remove(genericSpare.length - 1)
      _totalFree -= 1
      _genericHits += 1
      frame.setSignature(sig)
      borrowed += frame
      frame
    } else {
      _allocs += 1
      val frame = CapturedFrame.create(sig)
      borrowed += frame
      frame
    }
  }

  def endRun(): Unit = synchronized {
    var i = 0
    while (i < borrowed.length) {
      val frame = borrowed(i)
      frame.clearObjects()

      if (_totalFree >= maxRetained) {
        evictOne()
      }

      if (_totalFree < maxRetained) {
        val sig = frame.signature
        if (isWithinCaps(sig)) {
          val bucket = freeBySignature.getOrElseUpdate(sig, new ArrayBuffer[CapturedFrame]())
          bucket += frame
          _totalFree += 1
        } else if (genericSpare.length < MaxGenericSpare) {
          genericSpare += frame
          _totalFree += 1
        } else {
          _evictions += 1
        }
      } else {
        _evictions += 1
      }
      i += 1
    }
    borrowed.clear()
  }

  private def isWithinCaps(sig: CaptureSignature): Boolean = {
    sig.objectCount <= MaxArrayCap &&
      sig.booleanCount <= MaxArrayCap &&
      sig.intCount <= MaxArrayCap &&
      sig.longCount <= MaxArrayCap &&
      sig.shortCount <= MaxArrayCap &&
      sig.byteCount <= MaxArrayCap &&
      sig.charCount <= MaxArrayCap &&
      sig.doubleCount <= MaxArrayCap &&
      sig.floatCount <= MaxArrayCap
  }

  private def evictOne(): Unit = {
    if (genericSpare.nonEmpty) {
      genericSpare.remove(genericSpare.length - 1)
      _totalFree -= 1
      _evictions += 1
    } else {
      // Find largest bucket
      var largestBucket: ArrayBuffer[CapturedFrame] = null
      var maxCount = -1

      val it = freeBySignature.iterator
      while (it.hasNext) {
        val entry = it.next()
        val bucket = entry._2
        if (bucket.length > maxCount) {
          maxCount = bucket.length
          largestBucket = bucket
        }
      }

      if (largestBucket != null && largestBucket.nonEmpty) {
        largestBucket.remove(largestBucket.length - 1)
        _totalFree -= 1
        _evictions += 1
      }
    }
  }

  def clear(): Unit = synchronized {
    freeBySignature.clear()
    genericSpare.clear()
    borrowed.clear()
    _totalFree = 0
  }

  def totalFree: Int = synchronized(_totalFree)

  def hits: Long = synchronized(_hits)

  def genericHits: Long = synchronized(_genericHits)

  def allocs: Long = synchronized(_allocs)

  def evictions: Long = synchronized(_evictions)
}
