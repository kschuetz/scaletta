package software.kes.scaletta.ast

sealed trait Concat

object Concat {
  case class Segment(prefix: String,
                     expression: Expression,
                     next: Concat) extends Concat

  case class Suffix(value: String) extends Concat
}
