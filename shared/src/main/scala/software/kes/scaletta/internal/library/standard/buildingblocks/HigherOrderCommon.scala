package software.kes.scaletta.internal.library.standard.buildingblocks

import software.kes.scaletta.api.{ArgumentReader, NativeStep}
import software.kes.scaletta.internal.library.standard.higherorder.Step

object HigherOrderCommon {

  trait Strategy[C, R] {
    type State

    def initialState(input: C): Step[C, State]

    def nextState(state: State, functionCallResult: R): Step[C, State]
  }

  /**
   * @param convertFunctionOutput first parameter is the input that was passed into the function,
   *                              second parameter is output from the function
   */
  def create[C, R](convertFunctionOutput: (Any, Any) => R)
                  (strategy: Strategy[C, R]): ArgumentReader => NativeStep =
    (argumentReader: ArgumentReader) => {
      val input = argumentReader.readObject(0).asInstanceOf[C]
      strategy.initialState(input) match {
        case Step.Done(result) => NativeStep.done(result)
        case Step.ApplyFunction(state, elem) =>
          val fn = argumentReader.unsafeReadFunction(1)

          def go(state: strategy.State, elem: Any): NativeStep = {
            fn.setArgument(0, elem)
            NativeStep.call(fn, mapped => {
              strategy.nextState(state, convertFunctionOutput(elem, mapped)) match {
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
