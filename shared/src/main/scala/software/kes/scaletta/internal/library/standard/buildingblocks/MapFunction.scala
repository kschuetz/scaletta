package software.kes.scaletta.internal.library.standard.buildingblocks

import software.kes.scaletta.api.{ArgumentReader, NativeStep}

object MapFunction {
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

  trait Strategy[C] {
    type State

    def initialState(input: C): Step[C, State]

    def nextState(state: State, mappedElement: Any): Step[C, State]
  }

  def create[C](strategy: Strategy[C]): ArgumentReader => NativeStep =
    (argumentReader: ArgumentReader) => {
      val input = argumentReader.readObject(0).asInstanceOf[C]
      strategy.initialState(input) match {
        case Step.Done(result) => NativeStep.done(result)
        case Step.ApplyFunction(state, elem) =>
          val fn = argumentReader.unsafeReadFunction(1)

          def go(state: strategy.State, elem: Any): NativeStep = {
            fn.setArgument(0, elem)
            NativeStep.call(fn, mapped => {
              strategy.nextState(state, mapped) match {
                case Step.Done(result) => NativeStep.done(result)
                case Step.ApplyFunction(state, elem) =>
                  go(state, elem)
              }
            })
          }

          go(state, elem)
      }
    }
}
