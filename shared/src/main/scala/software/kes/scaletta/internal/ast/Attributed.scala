package software.kes.scaletta.internal.ast

import software.kes.scaletta.api.{Position, ProperType, TypeId}
import software.kes.scaletta.internal.reporting.{CharIndex, LineMap, Pos}
import software.kes.scaletta.util.functional.Functor

final case class Attributed[A](value: A,
                               begin: CharIndex,
                               end: CharIndex,
                               properType: ProperType[TypeId]) {
  def map[B](fn: A => B): Attributed[B] =
    Attributed(fn(value), begin, end, properType)

  def as[B](newValue: B): Attributed[B] =
    Attributed(newValue, begin, end, properType)

  def withType(newType: ProperType[TypeId]): Attributed[A] =
    Attributed(value, begin, end, newType)

  def pos: Pos[A] = Pos(value, begin, end)

  def toPosition(implicit lineMap: LineMap): Position = lineMap.indexToPosition(begin)

  def toEndPosition(implicit lineMap: LineMap): Position = lineMap.indexToPosition(end)

  def positionTuple: (Int, Int) = (begin.value, end.value)
}

object Attributed {
  def apply[A](value: A, pos: Pos[_], properType: ProperType[TypeId]): Attributed[A] =
    Attributed(value, pos.begin, pos.end, properType)

  implicit object attributedFunctor extends Functor[Attributed] {
    def map[A, B](fa: Attributed[A])(f: A => B): Attributed[B] = fa.map(f)
  }
}
