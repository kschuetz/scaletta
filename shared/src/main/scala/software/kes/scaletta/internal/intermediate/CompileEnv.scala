package software.kes.scaletta.internal.intermediate

import software.kes.scaletta.common.BasicType

sealed trait BindingInfo

object BindingInfo {
  final case class Val(absoluteIndex: Int) extends BindingInfo

  final case class LazyVal(absoluteIndex: Int, functionIndex: Int, basicType: BasicType) extends BindingInfo

  final case class Def(functionIndex: Int, returnType: BasicType) extends BindingInfo
}

final case class CompileEnv(layers: List[Vector[BindingInfo]],
                            nextVarIndex: Int) {
  def resolve(scope: Int, slot: Int): BindingInfo = layers(scope)(slot)

  def pushLayer(layer: Vector[BindingInfo], newVarCount: Int): CompileEnv =
    copy(layers = layer :: layers, nextVarIndex = nextVarIndex + newVarCount)
}

object CompileEnv {
  def empty: CompileEnv = CompileEnv(Nil, 0)
}
