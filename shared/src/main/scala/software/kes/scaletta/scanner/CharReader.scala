package software.kes.scaletta.scanner

import software.kes.scaletta.scanner.CharReader.Settings
import software.kes.scaletta.util.{CharPushback, SettingsStack}

object CharReader {
  def create(source: Iterator[Char],
             lineMapBuilder: LineMapBuilder,
             currentIndex: CharIndex = CharIndex(0),
             settings: Settings = Settings()): CharReader = {
    val pushback = CharPushback.create()
    new CharReader(source, pushback, currentIndex, currentIndex, lineMapBuilder, SettingsStack.create(settings))
  }

  case class Settings(normalizeNewLines: Boolean = true)
}

final class CharReader private(source: Iterator[Char],
                               pushback: CharPushback,
                               private var _currentIndex: CharIndex,
                               private var highWater: CharIndex,
                               private val lineMapBuilder: LineMapBuilder,
                               private val settingsStack: SettingsStack[Settings]) {
  private var lastReadWidth: Int = 1

  def get(): Option[Char] =
    if (pushback.nonEmpty) {
      val w = pushback.peekWidth()
      _currentIndex += w
      lastReadWidth = w
      Some(pushback.pop())
    } else if (source.hasNext) {
      var result = source.next()
      if (settings.normalizeNewLines && result == '\r') {
        if (source.hasNext) {
          val next = source.next()
          if (next == '\n') {
            _currentIndex += 2
            lastReadWidth = 2
          } else {
            _currentIndex += 1
            lastReadWidth = 1
            pushback.push(next, isDoubleWidth = false)
          }
        } else {
          _currentIndex += 1
          lastReadWidth = 1
        }
        if (highWater < _currentIndex) {
          lineMapBuilder.addLineBegin(_currentIndex)
          highWater = _currentIndex
        }
        result = '\n'
      } else if (result == '\n') {
        _currentIndex += 1
        lastReadWidth = 1
        if (highWater < _currentIndex) {
          lineMapBuilder.addLineBegin(_currentIndex)
          highWater = _currentIndex
        }
      } else {
        _currentIndex += 1
        lastReadWidth = 1
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

  def matchSequence(cs: Iterable[Char]): Boolean = {
    val iter = cs.iterator
    var stack = List.empty[Char]
    var result = true
    while (result && iter.hasNext) {
      val ch = iter.next()
      if (!tryGet(ch)) {
        result = false
      } else stack = ch :: stack
    }
    if (!result) stack.foreach(unget)
    result
  }

  def skipWhile(p: Char => Boolean): Unit = {
    var loop = true
    while (loop) {
      get() match {
        case Some(ch) =>
          if (!p(ch)) {
            unget(ch)
            loop = false
          }
        case None => loop = false
      }
    }
  }

  def skipUntil(p: Char => Boolean): Unit = {
    var loop = true
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
      if (settings.normalizeNewLines && result == '\r') {
        if (source.hasNext) {
          val next = source.next()
          if (next == '\n') {
            pushback.push('\n', isDoubleWidth = true)
          } else {
            pushback.push('\n', isDoubleWidth = false)
            pushback.push(next, isDoubleWidth = false)
          }
        } else {
          pushback.push('\n', isDoubleWidth = false)
        }
        Some('\n')
      } else {
        pushback.push(result, isDoubleWidth = false)
        Some(result)
      }
    } else None

  def unget(ch: Char): Unit = {
    pushback.push(ch, isDoubleWidth = lastReadWidth == 2)
    _currentIndex -= lastReadWidth
  }

  def ungetString(s: String): Unit =
    s.reverseIterator.foreach(unget)

  def currentIndex: CharIndex = _currentIndex

  def prevIndex: CharIndex = _currentIndex - 1

  def settings: CharReader.Settings = settingsStack.current

  /**
   * Modifies the settings in place. Does not affect the settings stack.
   */
  def modifySettings(fn: Settings => Settings): Unit =
    settingsStack.modify(fn)

  /**
   * Pushes the current settings onto the stack, then modifies the active settings.
   * Should eventually be matched with a call to popSettings().
   */
  def pushSettings(fn: Settings => Settings): Unit = {
    settingsStack.push(fn)
  }

  /**
   * Pops the topmost settings from the stack, restoring the previous settings.
   * Should be matched with a call to pushSettings().
   */
  def popSettings(): Unit =
    settingsStack.pop()
}
