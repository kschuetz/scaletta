package software.kes.scaletta.internal.reporting

import software.kes.scaletta.api.Position
import software.kes.scaletta.util.functional.Functor

object Pos {
  def apply[A](value: A, index: CharIndex): Pos[A] =
    Pos(value, index, index)

  implicit object posFunctor extends Functor[Pos] {
    def map[A, B](fa: Pos[A])(f: A => B): Pos[B] = fa.map(f)
  }
}

case class Pos[A](value: A,
                  begin: CharIndex,
                  end: CharIndex) {
  def map[B](fn: A => B): Pos[B] =
    Pos(fn(value), begin, end)

  def as[B](value: B): Pos[B] =
    Pos(value, begin, end)

  def toPosition(implicit lineMap: LineMap): Position = lineMap.indexToPosition(begin)

  def toEndPosition(implicit lineMap: LineMap): Position = lineMap.indexToPosition(end)

  def positionTuple: (Int, Int) = (begin.value, end.value)
}
