package software.kes.scaletta.util

object SettingsStack {
  def create[Settings](initial: Settings): SettingsStack[Settings] =
    new SettingsStack(initial, List.empty)
}

final class SettingsStack[Settings] private(private var _current: Settings,
                                            private var stack: List[Settings]) {

  def current: Settings = _current

  /**
   * Modifies the current settings in place. Does not affect the settings stack.
   */
  def modify(fn: Settings => Settings): Unit =
    _current = fn(current)

  /**
   * Pushes the current settings onto the stack, then modifies the current settings in place.
   * Should eventually be matched with a call to pop().
   */
  def push(fn: Settings => Settings): Unit = {
    stack = current :: stack
    _current = fn(current)
  }

  /**
   * Pops the topmost settings from the stack, restoring the previous settings.
   * Should be matched with a call to pushSettings().
   */
  def pop(): Unit =
    stack match {
      case head :: tail =>
        _current = head
        stack = tail
      case Nil =>
        throw new IllegalStateException("pop() called on empty stack")
    }

}
