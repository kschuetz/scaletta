package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api._

import scala.collection.mutable

object Collections {

  lazy val module: ScalettaModule[Unit] =
    ScalettaModule { setup =>
      val st = setup.standardTypes
      val registry = setup.methodRegistry

      Iterables.registerMethods(registry, st)(st.ListT, list)
      Iterables.registerMethods(registry, st)(st.VectorT, vector)
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
