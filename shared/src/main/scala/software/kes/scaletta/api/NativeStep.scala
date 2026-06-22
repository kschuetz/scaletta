package software.kes.scaletta.api

sealed trait NativeStep

object NativeStep {
  def done(value: Any): NativeStep = value match {
    case value: Boolean => done(value)
    case value: Int => done(value)
    case value: Long => done(value)
    case value: Short => done(value)
    case value: Byte => done(value)
    case value: Char => done(value)
    case value: Double => done(value)
    case value: Float => done(value)
    case value: AnyRef => DoneObject(value)
  }

  def done(value: Boolean): NativeStep = if (value) DoneTrue else DoneFalse

  def done(value: Int): NativeStep = DoneInt(value)

  def done(value: Long): NativeStep = DoneLong(value)

  def done(value: Short): NativeStep = DoneShort(value)

  def done(value: Byte): NativeStep = DoneByte(value)

  def done(value: Char): NativeStep = DoneChar(value)

  def done(value: Double): NativeStep = DoneDouble(value)

  def done(value: Float): NativeStep = DoneFloat(value)

  sealed trait Call extends NativeStep {
    def target: CallTarget
  }

  case class CallObject(target: CallTarget, k: AnyRef => NativeStep) extends Call

  case class CallBoolean(target: CallTarget, k: Boolean => NativeStep) extends Call

  case class CallInt(target: CallTarget, k: Int => NativeStep) extends Call

  case class CallLong(target: CallTarget, k: Long => NativeStep) extends Call

  case class CallShort(target: CallTarget, k: Short => NativeStep) extends Call

  case class CallByte(target: CallTarget, k: Byte => NativeStep) extends Call

  case class CallChar(target: CallTarget, k: Char => NativeStep) extends Call

  case class CallDouble(target: CallTarget, k: Double => NativeStep) extends Call

  case class CallFloat(target: CallTarget, k: Float => NativeStep) extends Call

  sealed trait Done extends NativeStep

  case class DoneObject(value: AnyRef) extends Done

  sealed trait DoneBoolean extends Done {
    def value: Boolean
  }

  object DoneTrue extends DoneBoolean {
    def value: Boolean = true
  }

  object DoneFalse extends DoneBoolean {
    def value: Boolean = false
  }

  case class DoneInt(value: Int) extends Done

  case class DoneLong(value: Long) extends Done

  case class DoneShort(value: Short) extends Done

  case class DoneByte(value: Byte) extends Done

  case class DoneChar(value: Char) extends Done

  case class DoneDouble(value: Double) extends Done

  case class DoneFloat(value: Float) extends Done
}
