package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.ScalettaModule

object DataTypes {
  lazy val module: ScalettaModule[Unit] =
    ScalettaModule { setup =>
      val st = setup.standardTypes
      val registry = setup.methodRegistry

      Iterables.registerMethods(registry, st)(st.OptionT, option)
    }

  object option extends IterableBase {
    type C = Option[Any]
    type Builder = OptionBuilder[Any]

    protected def makeEmpty(): C = None

    protected def makeBuilder(): Builder = new OptionBuilder[Any](None)

    protected def addElement(builder: Builder, elem: Any): Unit = builder.addElement(elem)

    protected def makeResult(builder: Builder): C = builder.result
  }

  final class OptionBuilder[A](var result: Option[A]) {
    def addElement(elem: A): Unit = result = Some(elem)
  }
}
