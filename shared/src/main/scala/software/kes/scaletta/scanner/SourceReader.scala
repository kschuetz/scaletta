package software.kes.scaletta.scanner

import software.kes.scaletta.reporting.{CharIndex, LineMap, LineMapBuilder}
import software.kes.scaletta.scanner.SourceReader.Settings
import software.kes.scaletta.util.{CharPushback, SettingsStack}

object SourceReader {
  def create(source: Iterator[Char],
             lineMapBuilder: LineMapBuilder,
             currentIndex: CharIndex = CharIndex(0),
             settings: Settings = Settings()): SourceReader = {
    val pushback = CharPushback.create()
    new SourceReader(source, pushback, currentIndex, lineMapBuilder, SettingsStack.create(settings))
  }

  case class Settings()
}

final class SourceReader private(source: Iterator[Char],
                                 pushback: CharPushback,
                                 private var _currentIndex: CharIndex,
                                 private val lineMapBuilder: LineMapBuilder,
                                 private val settingsStack: SettingsStack[Settings]) {

  def get(): Option[Char] =
    fetchRaw() match {
      case Some(ch) =>
        _currentIndex += 1
        Some(ch)
      case None => None
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
      fetchRaw() match {
        case Some(other) =>
          pushback.push(other)
          Some(other)
        case None => None
      }
    } else None

  def unget(ch: Char): Unit = {
    pushback.push(ch)
    _currentIndex -= 1
  }

  def ungetString(s: String): Unit =
    s.reverseIterator.foreach(unget)

  def currentIndex: CharIndex = _currentIndex

  def lineMap: LineMap = lineMapBuilder.result

  def prevIndex: CharIndex = _currentIndex - 1

  def settings: SourceReader.Settings = settingsStack.current

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
   */
  private def fetchRaw(): Option[Char] =
    if (pushback.nonEmpty) {
      Some(pushback.pop())
    } else if (source.hasNext) {
      Some(source.next())
    } else None

  private[scanner] def recordNewline(atIndex: CharIndex): Unit =
    lineMapBuilder.addLineBegin(atIndex)
}
