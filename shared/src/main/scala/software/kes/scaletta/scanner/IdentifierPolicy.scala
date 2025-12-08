package software.kes.scaletta.scanner

object IdentifierPolicy {
  val DefaultMaxLength = Some(256)
}

trait IdentifierPolicy {
  def maxIdentifierLength: Option[Int]
}
