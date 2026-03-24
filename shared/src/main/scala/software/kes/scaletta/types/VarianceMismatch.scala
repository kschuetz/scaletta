package software.kes.scaletta.types

sealed trait VarianceMismatch {
  def variance: Variance

  def actual: VarianceRelationship

  def expected: VarianceRelationship
}

object VarianceMismatch {

  def check[T](variance: Variance, relationship: TypeRelationship[T]): Option[VarianceMismatch] =
    variance match {
      case Variance.Invariant => relationship match {
        case TypeRelationship.Same => None
        case TypeRelationship.StrictSubtype => Some(InvariantSubtype)
        case TypeRelationship.StrictSupertype => Some(InvariantSupertype)
        case _ => Some(InvariantUnrelated)
      }
      case Variance.Covariant => relationship match {
        case TypeRelationship.Same => None
        case TypeRelationship.StrictSubtype => None
        case TypeRelationship.StrictSupertype => Some(CovariantSupertype)
        case _ => Some(CovariantUnrelated)
      }
      case Variance.Contravariant => relationship match {
        case TypeRelationship.Same => None
        case TypeRelationship.StrictSubtype => Some(ContravariantSubtype)
        case TypeRelationship.StrictSupertype => None
        case _ => Some(ContravariantUnrelated)
      }
    }

  case object InvariantSubtype extends VarianceMismatch {
    def variance: Variance = Variance.Invariant

    def actual: VarianceRelationship = VarianceRelationship.Subtype

    def expected: VarianceRelationship = VarianceRelationship.Same
  }

  case object InvariantSupertype extends VarianceMismatch {
    def variance: Variance = Variance.Invariant

    def actual: VarianceRelationship = VarianceRelationship.Supertype

    def expected: VarianceRelationship = VarianceRelationship.Same
  }

  case object InvariantUnrelated extends VarianceMismatch {
    def variance: Variance = Variance.Invariant

    def actual: VarianceRelationship = VarianceRelationship.Unrelated

    def expected: VarianceRelationship = VarianceRelationship.Same
  }

  case object CovariantSupertype extends VarianceMismatch {
    def variance: Variance = Variance.Covariant

    def actual: VarianceRelationship = VarianceRelationship.Supertype

    def expected: VarianceRelationship = VarianceRelationship.Subtype
  }

  case object CovariantUnrelated extends VarianceMismatch {
    def variance: Variance = Variance.Covariant

    def actual: VarianceRelationship = VarianceRelationship.Unrelated

    def expected: VarianceRelationship = VarianceRelationship.Subtype
  }

  case object ContravariantSubtype extends VarianceMismatch {
    def variance: Variance = Variance.Contravariant

    def actual: VarianceRelationship = VarianceRelationship.Subtype

    def expected: VarianceRelationship = VarianceRelationship.Supertype
  }

  case object ContravariantUnrelated extends VarianceMismatch {
    def variance: Variance = Variance.Contravariant

    def actual: VarianceRelationship = VarianceRelationship.Unrelated

    def expected: VarianceRelationship = VarianceRelationship.Supertype
  }
}
