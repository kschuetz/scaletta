package software.kes.scaletta.parser

import software.kes.scaletta.parser.BindingPower.BindingPowerOrdering

import scala.annotation.tailrec

sealed trait BindingPower {
  /**
   * Bumps by a slight amount (negative or positive), but the result
   * will never touch the next level.
   */
  def nudge(amount: Int): BindingPower = {
    if (amount == 0) this
    else this match {
      case BindingPower.Nudge(bp, prevAmount) =>
        val newAmount = prevAmount + amount
        if (newAmount == 0) bp else BindingPower.Nudge(bp, newAmount)
      case _ => BindingPower.Nudge(this, amount)
    }
  }

  def >(other: BindingPower): Boolean = BindingPowerOrdering.gt(this, other)

  def >=(other: BindingPower): Boolean = BindingPowerOrdering.gteq(this, other)

  def <(other: BindingPower): Boolean = BindingPowerOrdering.lt(this, other)

  def <=(other: BindingPower): Boolean = BindingPowerOrdering.lteq(this, other)
}

object BindingPower {
  case object Minimum extends BindingPower

  case object LogicalOr extends BindingPower

  case object LogicalXor extends BindingPower

  case object LogicalAnd extends BindingPower

  case object Equality extends BindingPower

  case object Comparison extends BindingPower

  case object Colon extends BindingPower

  case object Addition extends BindingPower

  case object Multiplication extends BindingPower

  case object Alphanumeric extends BindingPower

  case object AllOthers extends BindingPower

  case class Nudge(bp: BindingPower, amount: Int) extends BindingPower

  implicit object BindingPowerOrdering extends Ordering[BindingPower] {
    override def compare(x: BindingPower, y: BindingPower): Int = {
      val x1 = major(x)
      val y1 = major(y)
      if (x1 != y1) x1.compareTo(y1)
      else minor(x).compareTo(minor(y))
    }

    @tailrec
    private def major(bindingPower: BindingPower): Int =
      bindingPower match {
        case Minimum => 0
        case LogicalOr => 2
        case LogicalXor => 3
        case LogicalAnd => 4
        case Equality => 5
        case Comparison => 6
        case Colon => 7
        case Addition => 8
        case Multiplication => 9
        case Alphanumeric => 10
        case AllOthers => 11
        case Nudge(bp, _) => major(bp)
      }

    private def minor(bindingPower: BindingPower): Int =
      bindingPower match {
        case Nudge(_, amount) => amount
        case _ => 0
      }
  }
}
