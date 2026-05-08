package software.kes.scaletta.api

import scala.language.implicitConversions

sealed trait ReceiverType

object ReceiverType {
  implicit def instance(typ: Type.Nominal[TypeId]): ReceiverType = Instance(typ)

  implicit def static(path: PackagePath.Absolute): ReceiverType = Static(path)

  /**
   * A method on an instance of a type. The first parameter (index 0) of the method will be the instance,
   * and the remaining parameters will be the method arguments. Actual method arguments will start at index 1.
   */
  case class Instance(typ: Type.Nominal[TypeId]) extends ReceiverType

  /**
   * A top-level method in a package. An instance argument will not be inserted before the method arguments.
   * Actual method arguments will start at index 0.
   */
  case class Static(path: PackagePath.Absolute) extends ReceiverType
}
