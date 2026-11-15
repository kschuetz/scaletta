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

  private object FilterStrategy extends FilterFunction.Strategy[C] {
    type State = MyState

    def initialState(input: C): Step[C, MyState] = {
      val iter = input.iterator
      if (iter.isEmpty) Step.done(input)
      else {
        val elem = iter.next()
        Step.applyFunction(new MyState(iter, makeBuilder()), elem)
      }
    }

    def nextState(state: MyState, functionCallResult: (Any, Boolean)): Step[C, MyState] = {
      val (value, keep) = functionCallResult
      if (keep) addElement(state.builder, value)
      if (state.iterator.hasNext)
        Step.applyFunction(state, state.iterator.next())
      else
        Step.done(makeResult(state.builder))
    }

    class MyState(val iterator: Iterator[Any],
                  val builder: Builder)
  }

  private object FlatMapStrategy extends FlatMapFunction.Strategy[C] {
    type State = MyState

    def initialState(input: C): Step[C, MyState] = {
      val iter = input.iterator
      if (iter.isEmpty) Step.done(input)
      else {
        val elem = iter.next()
        Step.applyFunction(new MyState(iter, makeBuilder()), elem)
      }
    }

    def nextState(state: MyState, functionCallResult: C): Step[C, MyState] = {
      functionCallResult.iterator.foreach { elem =>
        addElement(state.builder, elem)
      }
      if (state.iterator.hasNext)
        Step.applyFunction(state, state.iterator.next())
      else
        Step.done(makeResult(state.builder))
    }

    class MyState(val iterator: Iterator[Any],
                  val builder: Builder)
  }

  private object MapStrategy extends MapFunction.Strategy[C] {
    type State = MyState

    def initialState(input: C): Step[C, MyState] = {
      val iter = input.iterator
      if (iter.isEmpty) Step.done(input)
      else {
        val elem = iter.next()
        Step.applyFunction(new MyState(iter, makeBuilder()), elem)
      }
    }

    def nextState(state: MyState, functionCallResult: Any): Step[C, MyState] = {
      addElement(state.builder, functionCallResult)
      if (state.iterator.hasNext)
        Step.applyFunction(state, state.iterator.next())
      else
        Step.done(makeResult(state.builder))
    }

    class MyState(val iterator: Iterator[Any],
                  val builder: Builder)
  }
}
