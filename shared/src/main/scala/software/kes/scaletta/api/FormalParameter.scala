package software.kes.scaletta.api

import software.kes.scaletta.internal.runtime.CoreTypes

object FormalParameter {
  def any(name: Name, default: Option[Any] = None): FormalParameter =
    FormalParameter(name, CoreTypes.AnyT, default)

  def boolean(name: Name, default: Option[Boolean] = None): FormalParameter =
    FormalParameter(name, CoreTypes.BooleanT, default)

  def byte(name: Name, default: Option[Byte] = None): FormalParameter =
    FormalParameter(name, CoreTypes.ByteT, default)

  def char(name: Name, default: Option[Char] = None): FormalParameter =
    FormalParameter(name, CoreTypes.CharT, default)

  def double(name: Name, default: Option[Double] = None): FormalParameter =
    FormalParameter(name, CoreTypes.DoubleT, default)

  def float(name: Name, default: Option[Float] = None): FormalParameter =
    FormalParameter(name, CoreTypes.FloatT, default)

  def int(name: Name, default: Option[Int] = None): FormalParameter =
    FormalParameter(name, CoreTypes.IntT, default)

  def long(name: Name, default: Option[Long] = None): FormalParameter =
    FormalParameter(name, CoreTypes.LongT, default)

  def short(name: Name, default: Option[Short] = None): FormalParameter =
    FormalParameter(name, CoreTypes.ShortT, default)

  def string(name: Name, default: Option[String] = None): FormalParameter =
    FormalParameter(name, CoreTypes.StringT, default)
}

case class FormalParameter(name: Name,
                           typ: ProperType[TypeId],
                           default: Option[Any] = None) {
  def withDefault(value: Any): FormalParameter =
    copy(default = Some(value))
}  
