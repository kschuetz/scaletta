package software.kes.scaletta.internal.parser

sealed trait ParseWarning

object ParseWarning {
  case class SuspiciousInfixExpression(operator: String) extends ParseWarning
}
