package software.kes.scaletta.internal.library.standard.higherorder

import software.kes.scaletta.api._
import software.kes.scaletta.internal.library.standard.StandardTypes

object CommonHigherOrderMethods {
  case class Implementations(filterImpl: Option[ArgumentReader => NativeStep] = None,
                             flatMapImpl: Option[ArgumentReader => NativeStep] = None,
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
        MethodName(receiverType, Name("filter")),
        Vector(FormalParameter(Name("p"), Type.function[TypeId](typeA)(standardTypes.BooleanT))),
        tc.applyAll(typeA),
        FunctionImpl.higherOrder(impl)
      )
    }

    implementations.flatMapImpl.foreach { impl =>
      // flatMap[B](f: A => C[B]): C[B]
      registry.addMethod(
        MethodName(receiverType, Name("flatMap")),
        Vector(FormalParameter(Name("f"), Type.function[TypeId](typeA)(tc.applyAll(typeB)))),
        tc.applyAll(typeB),
        FunctionImpl.higherOrder(impl)
      )
    }

    implementations.mapImpl.foreach { impl =>
      // map[B](f: A => B): C[B]
      registry.addMethod(
        MethodName(receiverType, Name("map")),
        Vector(FormalParameter(Name("f"), Type.function[TypeId](typeA)(typeB))),
        tc.applyAll(typeB),
        FunctionImpl.higherOrder(impl)
      )
    }
  }
}
