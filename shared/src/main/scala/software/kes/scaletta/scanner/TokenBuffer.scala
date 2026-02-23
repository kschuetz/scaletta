package software.kes.scaletta.scanner

import scala.collection.mutable

object TokenBuffer {
  def create(): TokenBuffer = new TokenBuffer(mutable.Queue.empty)

  type Effect = TokenBuffer => Unit
}

final class TokenBuffer private(private val queue: mutable.Queue[Pos[Token]]) {
  def isEmpty: Boolean = queue.isEmpty

  def dequeue(): Pos[Token] = queue.dequeue()

  def enqueue(token: Pos[Token]): Unit = queue.enqueue(token)

  def get(index: Int): Pos[Token] = queue(index)

  def length: Int = queue.length

  def lastOption: Option[Pos[Token]] = queue.lastOption

  def updateEndOfInput(index: CharIndex): Unit = {
    if (!queue.lastOption.exists(_.value == Token.EndOfInput)) {
      enqueue(Pos(Token.EndOfInput, index, index))
    }
  }
}
