package software.kes.scaletta.internal.parser

import software.kes.scaletta.internal.parser.BindingPower.BindingPowerOrdering

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

  case object Lambda extends BindingPower

  case object Match extends BindingPower

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

  case object MemberAccess extends BindingPower

  case class Nudge(bp: BindingPower, amount: Int) extends BindingPower

  /**
   * All base precedence levels in increasing order of precedence.
   */
  val allBaseLevels: Vector[BindingPower] = Vector(
    Minimum,
    Lambda,
    Match,
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
    MemberAccess
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
        case Lambda => 1
        case Match => 2
        case Ascription => 3
        case Alphanumeric => 4
        case LogicalOr => 5
        case LogicalXor => 6
        case LogicalAnd => 7
        case Equality => 8
        case Comparison => 9
        case ColonOperator => 10
        case Addition => 11
        case Multiplication => 12
        case OtherSymbolicOperators => 13
        case MemberAccess => 14
        case Nudge(bp, _) => major(bp)
      }

    private def minor(bindingPower: BindingPower): Int =
      bindingPower match {
        case Nudge(_, amount) => amount
        case _ => 0
      }
  }
}
