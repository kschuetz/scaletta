package software.kes.scaletta.types

import software.kes.scaletta.symbols.QualifiedName

trait TypeRegistryBootstrap extends TypeRegistry {
  def addTop(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addTopValue(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addTopRef(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addBottomRef(name: QualifiedName.Full): Type.Nominal[TypeId]

  def addBottom(name: QualifiedName.Full): Type.Nominal[TypeId]
}
