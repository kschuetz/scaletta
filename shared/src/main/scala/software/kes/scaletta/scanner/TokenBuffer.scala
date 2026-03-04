package software.kes.scaletta.scanner

import software.kes.scaletta.reporting.Pos

import scala.collection.mutable

object TokenBuffer {
  def create(): TokenBuffer = new TokenBuffer(mutable.Queue.empty)

  type Effect = TokenBuffer => Unit
}

final class TokenBuffer private(private val queue: mutable.Queue[Pos[Token]]) {
  private var _terminalToken: Option[Pos[Token]] = None

  def isEmpty: Boolean = queue.isEmpty && _terminalToken.isEmpty

  def isExhausted: Boolean = _terminalToken.isDefined

  def dequeue(): Pos[Token] =
    if (queue.nonEmpty) {
      queue.dequeue()
    } else {
      _terminalToken.getOrElse(throw new NoSuchElementException("dequeue from empty buffer"))
    }

  def enqueue(token: Pos[Token]): Unit = {
    if (_terminalToken.isEmpty) {
      queue.enqueue(token)
    }
  }

  def get(index: Int): Pos[Token] = {
    if (index < queue.length) {
      queue(index)
    } else {
      _terminalToken.getOrElse(throw new IndexOutOfBoundsException(index.toString))
    }
  }

  def length: Int = queue.length

  def mostRecentlyAdded: Option[Pos[Token]] =
    if (queue.nonEmpty) Some(queue.last)
    else _terminalToken

  def terminate(pos: Pos[Token]): Unit = {
    if (_terminalToken.isEmpty) {
      _terminalToken = Some(pos)
    }
  }
}
