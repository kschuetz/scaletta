package software.kes.scaletta.parser

sealed trait ParseWarning

object ParseWarning {
  case class SuspiciousInfixExpression(operator: String) extends ParseWarning
}
