package software.kes.scaletta.types

import software.kes.scaletta.symbols.QualifiedName

trait TypeRegistryBootstrap extends TypeRegistry {
  /**
   * There must be exactly one top type (e.g., Any).
   */
  def setTop(name: QualifiedName.Full): Type.Nominal[TypeId]

  /**
   * There must be at most one top value type (e.g., AnyVal).
   */
  def setTopValue(name: QualifiedName.Full): Type.Nominal[TypeId]

  /**
   * There must be at most one top ref type (e.g., AnyRef).
   */
  def setTopRef(name: QualifiedName.Full): Type.Nominal[TypeId]

  /**
   * There must be at most one bottom ref type (e.g., Null).
   */
  def setBottomRef(name: QualifiedName.Full): Type.Nominal[TypeId]

  /**
   * There must be at most one bottom type (e.g., Nothing).
   */
  def setBottom(name: QualifiedName.Full): Type.Nominal[TypeId]
}
