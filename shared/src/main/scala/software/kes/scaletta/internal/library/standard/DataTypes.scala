package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.ScalettaModule
import software.kes.scaletta.internal.library.standard.buildingblocks.{FlatMapFunction, MapFunction}
import software.kes.scaletta.internal.library.standard.higherorder.Step

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

  private object OptionMapStrategy extends MapFunction.Strategy[Option[Any]] {
    type State = Unit

    def initialState(input: Option[Any]): Step[Option[Any], Unit] =
      input match {
        case Some(value) => Step.applyFunction((), value)
        case None => Step.done(None)
      }

    def nextState(state: Unit, mappedElement: Any): Step[Option[Any], Unit] =
      Step.done(Some(mappedElement))
  }

  private object OptionFlatMapStrategy extends FlatMapFunction.Strategy[Option[Any]] {
    type State = Unit

    def initialState(input: Option[Any]): Step[Option[Any], Unit] =
      input match {
        case Some(value) => Step.applyFunction((), value)
        case None => Step.done(None)
      }

    def nextState(state: Unit, mappedElement: Option[Any]): Step[Option[Any], Unit] =
      Step.done(mappedElement)
  }

  private object EitherMapStrategy extends MapFunction.Strategy[Either[Any, Any]] {
    type State = Unit

    def initialState(input: Either[Any, Any]): Step[Either[Any, Any], Unit] =
      input match {
        case Right(value) => Step.applyFunction((), value)
        case left => Step.done(left)
      }

    def nextState(state: Unit, mappedElement: Any): Step[Either[Any, Any], Unit] =
      Step.done(Right(mappedElement))
  }

  private object EitherFlatMapStrategy extends FlatMapFunction.Strategy[Either[Any, Any]] {
    type State = Unit

    def initialState(input: Either[Any, Any]): Step[Either[Any, Any], Unit] =
      input match {
        case Right(value) => Step.applyFunction((), value)
        case left => Step.done(left)
      }

    def nextState(state: Unit, mappedElement: Either[Any, Any]): Step[Either[Any, Any], Unit] =
      Step.done(mappedElement)
  }
}
