package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.{ArgumentReader, NativeStep}

import scala.collection.mutable

object Collections {

  trait IterableBase {
    type C <: Iterable[_]

    type Builder

    protected def makeEmpty(): C

    protected def makeBuilder(): Builder

    protected def addElement(builder: Builder, elem: Any): Unit

    protected def makeResult(builder: Builder): C

    def filterImpl(argumentReader: ArgumentReader): NativeStep = {
      val input = argumentReader.unsafeReadObject(0).asInstanceOf[C]
      if (input.isEmpty) NativeStep.done(makeEmpty())
      else {
        val fn = argumentReader.unsafeReadFunction(1)
        val builder = makeBuilder()
        val iter = input.iterator

        def go(): NativeStep = {
          if (iter.hasNext) {
            val elem = iter.next()
            fn.setArgument(0, elem)
            NativeStep.call(fn, keep => {
              if (keep.asInstanceOf[Boolean]) {
                addElement(builder, elem)
              }
              go()
            })
          } else NativeStep.done(makeResult(builder))
        }

        go()
      }
    }

    def mapImpl(argumentReader: ArgumentReader): NativeStep = {
      val input = argumentReader.unsafeReadObject(0).asInstanceOf[C]
      if (input.isEmpty) NativeStep.done(makeEmpty())
      else {
        val fn = argumentReader.unsafeReadFunction(1)
        val builder = makeBuilder()
        val iter = input.iterator

        def go(): NativeStep = {
          if (iter.hasNext) {
            val elem = iter.next()
            fn.setArgument(0, elem)
            NativeStep.call(fn, mapped => {
              addElement(builder, mapped)
              go()
            })
          } else NativeStep.done(makeResult(builder))
        }

        go()
      }
    }
  }

  object list extends IterableBase {
    type C = List[Any]
    type Builder = mutable.Builder[Any, List[Any]]

    protected def makeEmpty(): C = Nil

    protected def makeBuilder(): Builder = List.newBuilder[Any]

    protected def addElement(builder: Builder, elem: Any): Unit = builder += elem

    protected def makeResult(builder: Builder): C = builder.result()
  }

  object vector extends IterableBase {
    type C = Vector[Any]
    type Builder = mutable.Builder[Any, Vector[Any]]

    protected def makeEmpty(): C = Vector.empty[Any]

    protected def makeBuilder(): Builder = Vector.newBuilder[Any]

    protected def addElement(builder: Builder, elem: Any): Unit = builder += elem

    protected def makeResult(builder: Builder): C = builder.result()
  }
}
