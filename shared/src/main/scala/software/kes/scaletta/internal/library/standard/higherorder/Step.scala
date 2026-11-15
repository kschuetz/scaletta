package software.kes.scaletta.internal.library.standard.higherorder

sealed trait Step[C, State]

object Step {
  def done[C, State](result: C): Step[C, State] =
    Done(result)

  def applyFunction[C, State](state: State, elem: Any): Step[C, State] =
    ApplyFunction(state, elem)

  case class Done[C, State](result: C) extends Step[C, State]

  case class ApplyFunction[C, State](state: State,
                                     elem: Any) extends Step[C, State]
}
