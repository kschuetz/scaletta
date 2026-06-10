package software.kes.scaletta.internal.symbols

import software.kes.scaletta.internal.runtime.CoreTypes
import software.kes.scaletta.internal.symbols.SignatureQuery.Group

object SignatureQuery {
  val empty: SignatureQuery = SignatureQuery.ofGroups()

  def of(param: SignatureQueryParameter, more: SignatureQueryParameter*): SignatureQuery =
    ofGroups(Group(Vector(param) ++ more))

  def ofGroups(group: Group*): SignatureQuery =
    new SignatureQuery(group.toVector)

  val any: SignatureQuery = SignatureQuery.of(CoreTypes.AnyT)
  val anyRef: SignatureQuery = SignatureQuery.of(CoreTypes.AnyRefT)
  val boolean: SignatureQuery = SignatureQuery.of(CoreTypes.BooleanT)
  val byte: SignatureQuery = SignatureQuery.of(CoreTypes.ByteT)
  val char: SignatureQuery = SignatureQuery.of(CoreTypes.CharT)
  val double: SignatureQuery = SignatureQuery.of(CoreTypes.DoubleT)
  val float: SignatureQuery = SignatureQuery.of(CoreTypes.FloatT)
  val int: SignatureQuery = SignatureQuery.of(CoreTypes.IntT)
  val long: SignatureQuery = SignatureQuery.of(CoreTypes.LongT)
  val short: SignatureQuery = SignatureQuery.of(CoreTypes.ShortT)
  val string: SignatureQuery = SignatureQuery.of(CoreTypes.StringT)

  case class Group(parameters: Vector[SignatureQueryParameter])
}

final class SignatureQuery private(val groups: Vector[Group]) {
  def addGroup(params: SignatureQueryParameter*): SignatureQuery =
    addGroup(Group(params.toVector))

  def addGroup(group: Group): SignatureQuery =
    new SignatureQuery(groups :+ group)

  override def equals(other: Any): Boolean = other match {
    case that: SignatureQuery =>
      groups == that.groups
    case _ => false
  }

  override def hashCode(): Int = groups.hashCode()

  override def toString = s"SignatureQuery($groups)"
}
