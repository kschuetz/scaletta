package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api._

import scala.collection.mutable

object Collections {

  lazy val module: ScalettaModule[Unit] =
    ScalettaModule { setup =>
      val st = setup.standardTypes
      val registry = setup.methodRegistry

      def registerMethods(constructor: Type.Constructor[TypeId], base: IterableBase): Unit = {
        val typeA = Type.variable(0)
        val typeB = Type.variable(1)

        val receiverType = ReceiverType.instance(Type.Nominal(constructor.name))
        val tc = TypeApplier.fromNode(constructor)

        // map[B](f: A => B): List[B]
        registry.addMethod(
          MethodName(receiverType, Name("map")),
          Vector(FormalParameter(Name("f"), Type.function[TypeId](typeA)(typeB))),
          tc.applyAll(typeB),
          FunctionImpl.higherOrder(base.mapImpl)
        )

        // flatMap[B](f: A => List[B]): List[B]
        registry.addMethod(
          MethodName(receiverType, Name("flatMap")),
          Vector(FormalParameter(Name("f"), Type.function[TypeId](typeA)(tc.applyAll(typeB)))),
          tc.applyAll(typeB),
          FunctionImpl.higherOrder(base.flatMapImpl)
        )

        // filter(f: A => Boolean): List[A]
        registry.addMethod(
          MethodName(receiverType, Name("filter")),
          Vector(FormalParameter(Name("p"), Type.function[TypeId](typeA)(st.BooleanT))),
          tc.applyAll(typeA),
          FunctionImpl.higherOrder(base.filterImpl)
        )
      }

      registerMethods(st.ListT, list)
      registerMethods(st.VectorT, vector)
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
