package software.kes.scaletta.internal.scanner

object IdentifierPolicy {
  val DefaultMaxLength = Some(256)

  case object Default extends IdentifierPolicy {
    def maxIdentifierLength: Option[Int] = DefaultMaxLength
  }
}

trait IdentifierPolicy {
  def maxIdentifierLength: Option[Int]
}
