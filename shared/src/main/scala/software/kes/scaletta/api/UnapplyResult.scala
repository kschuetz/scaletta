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

  case class Success(extractedValues: Iterable[Any]) extends UnapplyResult

  case object Failure extends UnapplyResult
}
