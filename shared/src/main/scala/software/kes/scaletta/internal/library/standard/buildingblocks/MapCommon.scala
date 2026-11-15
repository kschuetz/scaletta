package software.kes.scaletta.internal.library.standard.buildingblocks

import software.kes.scaletta.api.{ArgumentReader, NativeStep}
import software.kes.scaletta.internal.library.standard.higherorder.Step

object MapCommon {

  trait Strategy[C, Mapped] {
    type State

    def initialState(input: C): Step[C, State]

    def nextState(state: State, mappedElement: Mapped): Step[C, State]
  }

  def create[C, Mapped](strategy: Strategy[C, Mapped]): ArgumentReader => NativeStep =
    (argumentReader: ArgumentReader) => {
      val input = argumentReader.readObject(0).asInstanceOf[C]
      strategy.initialState(input) match {
        case Step.Done(result) => NativeStep.done(result)
        case Step.ApplyFunction(state, elem) =>
          val fn = argumentReader.unsafeReadFunction(1)

          def go(state: strategy.State, elem: Any): NativeStep = {
            fn.setArgument(0, elem)
            NativeStep.call(fn, mapped => {
              strategy.nextState(state, mapped.asInstanceOf[Mapped]) match {
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
