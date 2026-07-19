package software.kes.scaletta.util.conversions

object CollectionToTuple {
  val MaxArity = 22

  /**
   * Converts an array to a tuple of arity up to 22.
   * If the array is empty, the unit tuple () is returned.
   * If the array has only one element, that element is returned directly.
   * If the array has 2..22 elements, they are returned as a tuple.
   * Anything elements beyond 22 will be ignored.
   */
  def arrayToTuple(arr: Array[_]): Any = {
    val iter = arr.iterator

    def x: Any = iter.next()

    val arity = arr.length

    arity match {
      case 0 => ()
      case 1 => x
      case 2 => (x, x)
      case 3 => (x, x, x)
      case 4 => (x, x, x, x)
      case 5 => (x, x, x, x, x)
      case 6 => (x, x, x, x, x, x)
      case 7 => (x, x, x, x, x, x, x)
      case 8 => (x, x, x, x, x, x, x, x)
      case 9 => (x, x, x, x, x, x, x, x, x)
      case 10 => (x, x, x, x, x, x, x, x, x, x)
      case 11 => (x, x, x, x, x, x, x, x, x, x, x)
      case 12 => (x, x, x, x, x, x, x, x, x, x, x, x)
      case 13 => (x, x, x, x, x, x, x, x, x, x, x, x, x)
      case 14 => (x, x, x, x, x, x, x, x, x, x, x, x, x, x)
      case 15 => (x, x, x, x, x, x, x, x, x, x, x, x, x, x, x)
      case 16 => (x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x)
      case 17 => (x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x)
      case 18 => (x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x)
      case 19 => (x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x)
      case 20 => (x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x)
      case 21 => (x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x)
      case _ => (x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x)
    }
  }
}
