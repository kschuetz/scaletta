package software.kes.scaletta.internal.types

sealed trait ConjunctionType {
  def operator: String
}

object ConjunctionType {
  case object Union extends ConjunctionType {
    def operator: String = "|"
  }

  case object Intersection extends ConjunctionType {
    def operator: String = "&"
  }
}
