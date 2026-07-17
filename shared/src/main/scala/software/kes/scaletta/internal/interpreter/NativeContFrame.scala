package software.kes.scaletta.internal.interpreter

import software.kes.scaletta.api.NativeStep

private[interpreter] sealed trait NativeContFrame

private[interpreter] object NativeContFrame {
  final case class HigherOrderCont(k: Any => NativeStep,
                                   resultTypeTag: Byte) extends NativeContFrame
}
