package software.kes.scaletta.internal.interpreter

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
  final val BranchUnless = 12
  final val Dup = 13
  final val Swap = 14
  final val CallNative = 15
  final val CallLocal = 16
  final val TailCallLocal = 17
  final val Return = 18
  final val Pop = 19
  final val LogicalAnd = 20
  final val LogicalOr = 21
  final val Box = 22
  final val Convert = 23
}
