package software.kes.scaletta.api

sealed trait NativeStep

object NativeStep {
  def call(target: CallTarget, k: Any => NativeStep): NativeStep = Call(target, k)

  def done(value: Any): NativeStep = Done(value)

  case class Call(target: CallTarget, k: Any => NativeStep) extends NativeStep

  case class Done(value: Any) extends NativeStep
}
