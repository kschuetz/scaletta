package software.kes.scaletta.parser

sealed trait ParseWarning

object ParseWarning {
  case class Generic(message: String) extends ParseWarning
}
