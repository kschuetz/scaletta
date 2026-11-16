package software.kes.scaletta.internal.library.standard.higherorder

import software.kes.scaletta.api._
import software.kes.scaletta.internal.library.standard.{CommonMethodNames, CommonParameterNames, StandardTypes}

object CommonHigherOrderMethods {

  case class FlatMap(impl: ArgumentReader => NativeStep,
                     customLambdaReturnType: Option[Type.Constructor[TypeId]] = None)

  case class Implementations(filterImpl: Option[ArgumentReader => NativeStep] = None,
                             flatMapImpl: Option[FlatMap] = None,
                             mapImpl: Option[ArgumentReader => NativeStep] = None,
                            )

  def registerMethods(registry: MethodRegistry,
                      standardTypes: StandardTypes)
                     (constructor: Type.Constructor[TypeId],
                      implementations: Implementations): Unit = {
    val typeA = Type.variable(0)
    val typeB = Type.variable(1)

    val receiverType = ReceiverType.instance(Type.Nominal(constructor.name))
    val tc = TypeApplier.fromNode(constructor)

    implementations.filterImpl.foreach { impl =>
      // filter(f: A => Boolean): C[A]
      registry.addMethod(
        MethodName(receiverType, CommonMethodNames.filter),
        Vector(FormalParameter(CommonParameterNames.p, Type.function[TypeId](typeA)(standardTypes.BooleanT))),
        tc.applyAll(typeA),
        FunctionImpl.higherOrder(impl)
      )
    }

    implementations.flatMapImpl.foreach { fm =>
      val lambdaReturnType = fm.customLambdaReturnType match {
        case Some(customReturnType) => TypeApplier.fromNode(customReturnType).applyAll(typeB)
        case None => tc.applyAll(typeB)
      }

      // flatMap[B](f: A => C[B]): C[B]
      registry.addMethod(
        MethodName(receiverType, CommonMethodNames.flatMap),
        Vector(FormalParameter(CommonParameterNames.f, Type.function[TypeId](typeA)(lambdaReturnType))),
        tc.applyAll(typeB),
        FunctionImpl.higherOrder(fm.impl)
      )
    }

    implementations.mapImpl.foreach { impl =>
      // map[B](f: A => B): C[B]
      registry.addMethod(
        MethodName(receiverType, CommonMethodNames.map),
        Vector(FormalParameter(CommonParameterNames.f, Type.function[TypeId](typeA)(typeB))),
        tc.applyAll(typeB),
        FunctionImpl.higherOrder(impl)
      )
    }
  }
}
