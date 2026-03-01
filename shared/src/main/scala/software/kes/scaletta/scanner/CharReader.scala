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
      case (Some('\r'), rawWidth) if settings.normalizeNewLines =>
        val (nextCh, nextWidth) = fetchRaw()
        nextCh match {
          case Some('\n') => advance(rawWidth + nextWidth)
          case Some(other) =>
            pushback.push(other, isDoubleWidth = nextWidth == 2)
            advance(rawWidth)
          case None => advance(rawWidth)
        }
        recordNewline(_currentIndex)
        Some('\n')

      case (Some('\n'), rawWidth) =>
        advance(rawWidth)
        recordNewline(_currentIndex)
        Some('\n')

      case (Some(other), rawWidth) =>
        advance(rawWidth)
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
      val (rawCh, rawWidth) = fetchRaw()
      rawCh match {
        case Some('\r') if settings.normalizeNewLines =>
          val (nextCh, nextWidth) = fetchRaw()
          nextCh match {
            case Some('\n') =>
              pushback.push('\n', isDoubleWidth = rawWidth + nextWidth == 2)
            case Some(other) =>
              pushback.push('\n', isDoubleWidth = rawWidth == 2)
              pushback.push(other, isDoubleWidth = nextWidth == 2)
            case None =>
              pushback.push('\n', isDoubleWidth = rawWidth == 2)
          }
          Some('\n')
        case Some(other) =>
          pushback.push(other, isDoubleWidth = rawWidth == 2)
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

  private def fetchRaw(): (Option[Char], Int) =
    if (pushback.nonEmpty) {
      val w = pushback.peekWidth()
      (Some(pushback.pop()), w)
    } else if (source.hasNext) {
      (Some(source.next()), 1)
    } else (None, 0)

  private def advance(width: Int): Unit = {
    _currentIndex += width
    lastReadWidth = width
  }

  private def recordNewline(atIndex: CharIndex): Unit =
    if (highWater < atIndex) {
      lineMapBuilder.addLineBegin(atIndex)
      highWater = atIndex
    }
}
