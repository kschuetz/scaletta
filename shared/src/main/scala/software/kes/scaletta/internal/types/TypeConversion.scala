package software.kes.scaletta.internal.types

sealed trait TypeConversion {
  def cost: Int
}

object TypeConversion {
  case object None extends TypeConversion {
    def cost: Int = Int.MaxValue
  }

  case object Identity extends TypeConversion {
    def cost: Int = 0
  }

  sealed trait Widening extends TypeConversion

  case object Widening1 extends Widening {
    def cost: Int = 1
  }

  case object Widening2 extends Widening {
    def cost: Int = 2
  }

  case object Widening3 extends Widening {
    def cost: Int = 3
  }

  case object Widening4 extends Widening {
    def cost: Int = 4
  }

  case object Widening5 extends Widening {
    def cost: Int = 5
  }
}
