package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.{ArgumentReader, NativeStep}

trait IterableBase {
  type C <: Iterable[_]

  type Builder

  protected def makeEmpty(): C

  protected def makeBuilder(): Builder

  protected def addElement(builder: Builder, elem: Any): Unit

  protected def makeResult(builder: Builder): C

  def filterImpl(argumentReader: ArgumentReader): NativeStep = {
    val input = argumentReader.readObject(0).asInstanceOf[C]
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

  def flatMapImpl(argumentReader: ArgumentReader): NativeStep = {
    val input = argumentReader.readObject(0).asInstanceOf[C]
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

  def mapImpl(argumentReader: ArgumentReader): NativeStep = {
    val input = argumentReader.readObject(0).asInstanceOf[C]
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
