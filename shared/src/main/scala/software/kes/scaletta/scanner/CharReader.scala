package software.kes.scaletta.scanner

import software.kes.scaletta.util.CharPushback

object CharReader {
  def create(source: Iterator[Char],
             lineMap: LineMap,
             currentIndex: CharIndex = CharIndex(0)): CharReader = {
    val pushback = CharPushback.create()
    new CharReader(source, pushback, currentIndex, currentIndex, lineMap)
  }
}

final class CharReader private(source: Iterator[Char],
                               pushback: CharPushback,
                               private var _currentIndex: CharIndex,
                               private var highWater: CharIndex,
                               private var lineMap: LineMap) {
  def get(): Option[Char] =
    if (pushback.nonEmpty) {
      _currentIndex += 1
      Some(pushback.pop())
    } else if (source.hasNext) {
      var result = source.next()
      if (result == '\r') {
        if (source.hasNext) {
          val next = source.next()
          if (next == '\n') {
            _currentIndex += 2
          } else {
            _currentIndex += 1
            pushback.push(next)
          }
        }
        if (highWater < _currentIndex) {
          lineMap = lineMap.addLineBegin(_currentIndex)
          highWater = _currentIndex
        }
        result = '\n'
      } else if (result == '\n') {
        _currentIndex += 1
        if (highWater < _currentIndex) {
          lineMap = lineMap.addLineBegin(_currentIndex)
          highWater = _currentIndex
        }
      } else {
        _currentIndex += 1
        if (highWater < _currentIndex) {
          highWater = _currentIndex
        }
      }
      Some(result)
    } else None

  def tryGet(ch: Char): Boolean =
    get() match {
      case Some(c) =>
        if (c == ch) true
        else {
          unget(c)
          false
        }
      case None => false
    }

  def skipUntil(p: Char => Boolean): Unit = {
    var loop = false
    while (loop) {
      get() match {
        case Some(ch) =>
          if (p(ch)) {
            unget(ch)
            loop = false
          }
        case None => loop = false
      }
    }
  }

  def peek(): Option[Char] =
    if (pushback.nonEmpty) {
      Some(pushback.peek())
    } else if (source.hasNext) {
      val result = source.next()
      pushback.push(result)
      Some(result)
    } else None

  def unget(ch: Char): Unit = {
    pushback.push(ch)
    _currentIndex -= 1
  }

  def currentIndex: CharIndex = _currentIndex

  def prevIndex: CharIndex = _currentIndex - 1
}
