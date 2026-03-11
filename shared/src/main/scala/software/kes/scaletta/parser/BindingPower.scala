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

  case object Ascription extends BindingPower

  case object Alphanumeric extends BindingPower

  case object LogicalOr extends BindingPower

  case object LogicalXor extends BindingPower

  case object LogicalAnd extends BindingPower

  case object Equality extends BindingPower

  case object Comparison extends BindingPower

  case object ColonOperator extends BindingPower

  case object Addition extends BindingPower

  case object Multiplication extends BindingPower

  case object OtherSymbolicOperators extends BindingPower

  case object PostfixCall extends BindingPower

  case class Nudge(bp: BindingPower, amount: Int) extends BindingPower

  /**
   * All base precedence levels in increasing order of precedence.
   */
  val allBaseLevels: Vector[BindingPower] = Vector(
    Minimum,
    Ascription,
    Alphanumeric,
    LogicalOr,
    LogicalXor,
    LogicalAnd,
    Equality,
    Comparison,
    ColonOperator,
    Addition,
    Multiplication,
    OtherSymbolicOperators,
    PostfixCall
  )

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
        case Ascription => 1
        case Alphanumeric => 2
        case LogicalOr => 3
        case LogicalXor => 4
        case LogicalAnd => 5
        case Equality => 6
        case Comparison => 7
        case ColonOperator => 8
        case Addition => 9
        case Multiplication => 10
        case OtherSymbolicOperators => 11
        case PostfixCall => 12
        case Nudge(bp, _) => major(bp)
      }

    private def minor(bindingPower: BindingPower): Int =
      bindingPower match {
        case Nudge(_, amount) => amount
        case _ => 0
      }
  }
}
