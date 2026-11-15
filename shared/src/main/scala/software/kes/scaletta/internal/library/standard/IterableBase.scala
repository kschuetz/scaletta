package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.{ArgumentReader, NativeStep}
import software.kes.scaletta.internal.library.standard.buildingblocks.{FilterFunction, MapFunction}

trait IterableBase {
  type C <: IterableOnce[_]

  type Builder

  protected def makeEmpty(): C

  protected def makeBuilder(): Builder

  protected def addElement(builder: Builder, elem: Any): Unit

  protected def makeResult(builder: Builder): C

  val filterImpl: ArgumentReader => NativeStep =
    FilterFunction.create(FilterStrategy)

  def flatMapImpl(argumentReader: ArgumentReader): NativeStep = {
    val input = argumentReader.readObject(0).asInstanceOf[C]
    val iter = input.iterator
    if (iter.isEmpty) NativeStep.done(input)
    else {
      val fn = argumentReader.unsafeReadFunction(1)
      val builder = makeBuilder()

      def go(): NativeStep = {
        if (iter.hasNext) {
          val elem = iter.next()
          fn.setArgument(0, elem)
          NativeStep.call(fn, mapped => {
            val inner = mapped.asInstanceOf[Iterable[_]]
            val innerIter = inner.iterator
            while (innerIter.hasNext) {
              addElement(builder, innerIter.next())
            }
            go()
          })
        } else NativeStep.done(makeResult(builder))
      }

      go()
    }
  }

  val mapImpl: ArgumentReader => NativeStep =
    MapFunction.create(MapStrategy)

  private object FilterStrategy extends FilterFunction.Strategy[C] {

    import FilterFunction.Step

    type State = MyState

    def initialState(input: C): Step[C, MyState] = {
      val iter = input.iterator
      if (iter.isEmpty) Step.done(input)
      else {
        val elem = iter.next()
        Step.applyPredicate(new MyState(iter, makeBuilder()), elem)
      }
    }

    def nextState(state: MyState, element: Any, keep: Boolean): Step[C, MyState] = {
      if (keep) addElement(state.builder, element)
      if (state.iterator.hasNext)
        Step.applyPredicate(state, state.iterator.next())
      else
        Step.done(makeResult(state.builder))
    }

    class MyState(val iterator: Iterator[Any],
                  val builder: Builder)
  }

  private object MapStrategy extends MapFunction.Strategy[C] {

    import MapFunction.Step

    type State = MyState

    def initialState(input: C): MapFunction.Step[C, MyState] = {
      val iter = input.iterator
      if (iter.isEmpty) Step.done(input)
      else {
        val elem = iter.next()
        Step.applyFunction(new MyState(iter, makeBuilder()), elem)
      }
    }

    def nextState(state: MyState, mappedElement: Any): MapFunction.Step[C, MyState] = {
      addElement(state.builder, mappedElement)
      if (state.iterator.hasNext)
        Step.applyFunction(state, state.iterator.next())
      else
        Step.done(makeResult(state.builder))
    }

    class MyState(val iterator: Iterator[Any],
                  val builder: Builder)
  }
}
