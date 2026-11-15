package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.{ArgumentReader, NativeStep}
import software.kes.scaletta.internal.library.standard.buildingblocks.{FilterFunction, FlatMapFunction, MapFunction}
import software.kes.scaletta.internal.library.standard.higherorder.Step

trait IterableBase {
  type C <: IterableOnce[_]

  type Builder

  protected def makeEmpty(): C

  protected def makeBuilder(): Builder

  protected def addElement(builder: Builder, elem: Any): Unit

  protected def makeResult(builder: Builder): C

  val filterImpl: ArgumentReader => NativeStep =
    FilterFunction.create(FilterStrategy)

  val flatMapImpl: ArgumentReader => NativeStep =
    FlatMapFunction.create(FlatMapStrategy)

  val mapImpl: ArgumentReader => NativeStep =
    MapFunction.create(MapStrategy)

  private abstract class HigherOrderStrategyBase {
    def initialState(input: C): Step[C, HigherOrderState] = {
      val iter = input.iterator
      if (iter.isEmpty) Step.done(input)
      else {
        val elem = iter.next()
        Step.applyFunction(new HigherOrderState(iter, makeBuilder()), elem)
      }
    }

    final protected def handleNext(state: HigherOrderState): Step[C, HigherOrderState] =
      if (state.iterator.hasNext)
        Step.applyFunction(state, state.iterator.next())
      else
        Step.done(makeResult(state.builder))
  }

  private object FilterStrategy extends HigherOrderStrategyBase with FilterFunction.Strategy[C] {
    type State = HigherOrderState

    def nextState(state: HigherOrderState, functionCallResult: (Any, Boolean)): Step[C, HigherOrderState] = {
      val (value, keep) = functionCallResult
      if (keep) addElement(state.builder, value)
      handleNext(state)
    }
  }

  private object FlatMapStrategy extends HigherOrderStrategyBase with FlatMapFunction.Strategy[C] {
    type State = HigherOrderState

    def nextState(state: HigherOrderState, functionCallResult: C): Step[C, HigherOrderState] = {
      functionCallResult.iterator.foreach { elem =>
        addElement(state.builder, elem)
      }
      handleNext(state)
    }
  }

  private object MapStrategy extends HigherOrderStrategyBase with MapFunction.Strategy[C] {
    type State = HigherOrderState

    def nextState(state: HigherOrderState, functionCallResult: Any): Step[C, HigherOrderState] = {
      addElement(state.builder, functionCallResult)
      handleNext(state)
    }
  }

  final class HigherOrderState(val iterator: Iterator[Any],
                               val builder: Builder)
}
