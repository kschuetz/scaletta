package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api._
import software.kes.scaletta.internal.library.standard.buildingblocks.{FilterFunction, FlatMapFunction, MapFunction}
import software.kes.scaletta.internal.library.standard.higherorder.CommonHigherOrderMethods.FlatMap
import software.kes.scaletta.internal.library.standard.higherorder.{CommonHigherOrderMethods, Step}

object DataTypes {
  lazy val module: ScalettaModule[Unit] =
    ScalettaModule { setup =>
      val st = setup.standardTypes
      val registry = setup.methodRegistry

      option.registerMethods(registry, st)
    }

  object option {

    def registerMethods(registry: MethodRegistry,
                        standardTypes: StandardTypes): Unit = {
      val optionT = standardTypes.OptionT
      CommonHigherOrderMethods.registerMethods(registry, standardTypes)(optionT, higherOrderImpls)

      val typeA = Type.variable(0)

      val receiverType = ReceiverType.instance(Type.Nominal(optionT.name))
      val tc = TypeApplier.fromNode(optionT)

      registry.addMethod(
        MethodName(receiverType, CommonMethodNames.withFilter),
        Vector(FormalParameter(CommonParameterNames.p, Type.function[TypeId](typeA)(standardTypes.BooleanT))),
        tc.applyAll(typeA),
        FunctionImpl.higherOrder(filterImpl)
      )
    }

    val filterImpl: ArgumentReader => NativeStep =
      FilterFunction.create(FilterStrategy)

    val flatMapImpl: ArgumentReader => NativeStep =
      FlatMapFunction.create(FlatMapStrategy)

    val mapImpl: ArgumentReader => NativeStep =
      MapFunction.create(MapStrategy)

    lazy val higherOrderImpls: CommonHigherOrderMethods.Implementations =
      CommonHigherOrderMethods.Implementations(
        filterImpl = Some(filterImpl),
        flatMapImpl = Some(FlatMap(flatMapImpl)),
        mapImpl = Some(mapImpl)
      )

    private object FilterStrategy extends FilterFunction.Strategy[Option[Any]] {
      type State = Unit

      def initialState(input: Option[Any]): Step[Option[Any], Unit] =
        input match {
          case Some(value) => Step.applyFunction((), value)
          case None => Step.done(None)
        }

      def nextState(state: Unit, functionCallResult: (Any, Boolean)): Step[Option[Any], Unit] = {
        val (value, keep) = functionCallResult
        if (keep) Step.done(Some(value)) else Step.done(None)
      }
    }

    private object FlatMapStrategy extends FlatMapFunction.Strategy[Option[Any], Option[Any]] {
      type State = Unit

      def initialState(input: Option[Any]): Step[Option[Any], Unit] =
        input match {
          case Some(value) => Step.applyFunction((), value)
          case None => Step.done(None)
        }

      def nextState(state: Unit, functionCallResult: Option[Any]): Step[Option[Any], Unit] =
        Step.done(functionCallResult)
    }

    private object MapStrategy extends MapFunction.Strategy[Option[Any]] {
      type State = Unit

      def initialState(input: Option[Any]): Step[Option[Any], Unit] =
        input match {
          case Some(value) => Step.applyFunction((), value)
          case None => Step.done(None)
        }

      def nextState(state: Unit, functionCallResult: Any): Step[Option[Any], Unit] =
        Step.done(Some(functionCallResult))
    }
  }


  private object EitherMapStrategy extends MapFunction.Strategy[Either[Any, Any]] {
    type State = Unit

    def initialState(input: Either[Any, Any]): Step[Either[Any, Any], Unit] =
      input match {
        case Right(value) => Step.applyFunction((), value)
        case left => Step.done(left)
      }

    def nextState(state: Unit, functionCallResult: Any): Step[Either[Any, Any], Unit] =
      Step.done(Right(functionCallResult))
  }

  private object EitherFlatMapStrategy extends FlatMapFunction.Strategy[Either[Any, Any], Either[Any, Any]] {
    type State = Unit

    def initialState(input: Either[Any, Any]): Step[Either[Any, Any], Unit] =
      input match {
        case Right(value) => Step.applyFunction((), value)
        case left => Step.done(left)
      }

    def nextState(state: Unit, functionCallResult: Either[Any, Any]): Step[Either[Any, Any], Unit] =
      Step.done(functionCallResult)
  }
}
