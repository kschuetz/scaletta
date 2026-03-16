package software.kes.scaletta.types

sealed trait TypeRelationship[+T] {
  def isSame: Boolean

  def isSubtype: Boolean

  def isSupertype: Boolean

  def isStrictSubtype: Boolean

  def isStrictSupertype: Boolean

  def commonSupertype: Option[T]
}

object TypeRelationship {
  /**
   * The two types are the same.
   */
  case object Same extends TypeRelationship[Nothing] {
    def isSame: Boolean = true

    def isSubtype: Boolean = true

    def isSupertype: Boolean = true

    def isStrictSubtype: Boolean = false

    def isStrictSupertype: Boolean = false

    def commonSupertype: Option[Nothing] = Option.empty
  }

  /**
   * The left type is a strict subtype of the right type.
   */
  case object StrictSubtype extends TypeRelationship[Nothing] {
    def isSame: Boolean = false

    def isSubtype: Boolean = true

    def isSupertype: Boolean = false

    def isStrictSubtype: Boolean = true

    def isStrictSupertype: Boolean = false

    def commonSupertype: Option[Nothing] = Option.empty
  }

  /**
   * The left type is a strict supertype of the right type.
   */
  case object StrictSupertype extends TypeRelationship[Nothing] {
    def isSame: Boolean = false

    def isSubtype: Boolean = false

    def isSupertype: Boolean = true

    def isStrictSubtype: Boolean = false

    def isStrictSupertype: Boolean = true

    def commonSupertype: Option[Nothing] = Option.empty
  }

  /**
   * The left and the right types have a common supertype.
   */
  case class HaveCommonSupertype[T](value: T) extends TypeRelationship[T] {
    def isSame: Boolean = false

    def isSubtype: Boolean = false

    def isSupertype: Boolean = false

    def isStrictSubtype: Boolean = false

    def isStrictSupertype: Boolean = false

    def commonSupertype: Option[T] = Some(value)
  }

  /**
   * No relationship between the two types.
   */
  case object None extends TypeRelationship[Nothing] {
    def isSame: Boolean = false

    def isSubtype: Boolean = false

    def isSupertype: Boolean = false

    def isStrictSubtype: Boolean = false

    def isStrictSupertype: Boolean = false

    def commonSupertype: Option[Nothing] = Option.empty
  }
}
