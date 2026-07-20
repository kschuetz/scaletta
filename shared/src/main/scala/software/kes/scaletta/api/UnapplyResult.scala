package software.kes.scaletta.api

sealed trait UnapplyResult

object UnapplyResult {
  def success(extractedValues: Iterable[Any]): UnapplyResult = {
    if (extractedValues.isEmpty) success0
    else Success(extractedValues)
  }

  def failure: UnapplyResult = Failure

  val success0: UnapplyResult = Success(Iterable.empty)

  def success1(arg: Any): UnapplyResult = Success(Iterable.single(arg))

  def success2(arg1: Any, arg2: Any): UnapplyResult = Success(Iterable(arg1, arg2))

  case class Success(extractedValues: Iterable[Any]) extends UnapplyResult

  case object Failure extends UnapplyResult
}
