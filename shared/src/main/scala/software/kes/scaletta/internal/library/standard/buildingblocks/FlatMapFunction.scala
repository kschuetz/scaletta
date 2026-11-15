package software.kes.scaletta.internal.library.standard.buildingblocks

import software.kes.scaletta.api.{ArgumentReader, NativeStep}
import software.kes.scaletta.internal.library.standard.higherorder.Step

object FlatMapFunction {
  trait Strategy[C] {
    type State

    def initialState(input: C): Step[C, State]

    def nextState(state: State, mappedElement: C): Step[C, State]
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
              strategy.nextState(state, mapped.asInstanceOf[C]) match {
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
