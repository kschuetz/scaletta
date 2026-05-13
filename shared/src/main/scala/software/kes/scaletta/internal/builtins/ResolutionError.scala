package software.kes.scaletta.internal.builtins

sealed trait ResolutionError

object ResolutionError {
  case object Ambiguous extends ResolutionError

  case object NotFound extends ResolutionError
}
