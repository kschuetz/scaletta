package software.kes.scaletta.interpreter

object Opcodes {
  final val Nop = 0
  final val PushConst = 1
  final val Push = 2
  final val StoreConst = 3
  final val Store = 4
  final val StoreWide = 5
  final val PushFromVar = 6
  final val PushFromVarWide = 7
  final val PopIntoVar = 8
  final val PopIntoVarWide = 9
  final val Branch = 10
  final val BranchIf = 11
  final val BranchIfNot = 12
  final val Dup = 13
  final val CallNative = 14
  final val CallLocal = 15
  final val Return = 16
  final val Pop = 17
  final val PopWide = 18
}
