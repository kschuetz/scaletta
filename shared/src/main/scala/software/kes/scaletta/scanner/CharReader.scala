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
    fetchRaw() match {
      case (Some('\r'), isDouble) if settings.normalizeNewLines =>
        val (nextCh, nextIsDouble) = fetchRaw()
        nextCh match {
          case Some('\n') => advance(isDouble, nextIsDouble)
          case Some(other) =>
            pushback.push(other, isDoubleWidth = nextIsDouble)
            advance(isDouble)
          case None => advance(isDouble)
        }
        recordNewline(_currentIndex)
        Some('\n')

      case (Some('\n'), isDouble) =>
        advance(isDouble)
        recordNewline(_currentIndex)
        Some('\n')

      case (Some(other), isDouble) =>
        advance(isDouble)
        if (highWater < _currentIndex) {
          highWater = _currentIndex
        }
        Some(other)

      case (None, _) => None
    }

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
      val (rawCh, rawIsDouble) = fetchRaw()
      rawCh match {
        case Some('\r') if settings.normalizeNewLines =>
          val (nextCh, nextIsDouble) = fetchRaw()
          nextCh match {
            case Some('\n') =>
              pushback.push('\n', isDoubleWidth = rawIsDouble || nextIsDouble)
            case Some(other) =>
              pushback.push('\n', isDoubleWidth = rawIsDouble)
              pushback.push(other, isDoubleWidth = nextIsDouble)
            case None =>
              pushback.push('\n', isDoubleWidth = rawIsDouble)
          }
          Some('\n')
        case Some(other) =>
          pushback.push(other, isDoubleWidth = rawIsDouble)
          Some(other)
        case None => None
      }
    } else None

  def unget(ch: Char): Unit = {
    pushback.push(ch, isDoubleWidth = lastReadWidth == 2)
    _currentIndex -= lastReadWidth
  }

  def ungetString(s: String): Unit =
    s.reverseIterator.foreach(unget)

  def currentIndex: CharIndex = _currentIndex

  def lineMap: LineMap = lineMapBuilder.result

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

  /**
   * Fetches the next character from the pushback buffer or the source iterator.
   * Returns a tuple containing the character (if any) and a boolean indicating
   * if it should be treated as a double-width character for index tracking.
   */
  private def fetchRaw(): (Option[Char], Boolean) =
    if (pushback.nonEmpty) {
      val isDouble = pushback.peekDoubleWidth()
      (Some(pushback.pop()), isDouble)
    } else if (source.hasNext) {
      (Some(source.next()), false)
    } else (None, false)

  private def advance(isDoubleWidth: Boolean): Unit = {
    val width = booleanToWidth(isDoubleWidth)
    _currentIndex += width
    lastReadWidth = width
  }

  private def advance(isDoubleWidth1: Boolean, isDoubleWidth2: Boolean): Unit = {
    val width = (booleanToWidth(isDoubleWidth1)) + (booleanToWidth(isDoubleWidth2))
    _currentIndex += width
    lastReadWidth = width
  }

  private def booleanToWidth(isDoubleWidth: Boolean): Int =
    if (isDoubleWidth) 2 else 1

  private def recordNewline(atIndex: CharIndex): Unit =
    if (highWater < atIndex) {
      lineMapBuilder.addLineBegin(atIndex)
      highWater = atIndex
    }
}
