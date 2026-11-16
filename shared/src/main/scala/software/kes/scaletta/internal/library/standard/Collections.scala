package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api._
import software.kes.scaletta.internal.library.standard.collection.WithFilterOps

import scala.collection.mutable

object Collections {

  lazy val module: ScalettaModule[Unit] =
    ScalettaModule { setup =>
      val st = setup.standardTypes
      val registry = setup.methodRegistry
      Iterables.registerMethods(registry, st)(st.ListT, list)
      Iterables.registerMethods(registry, st)(st.VectorT, vector)
      WithFilterOps.registerCollectionMethods(registry, st)(st.ListT, WithFilterOps.list)
      WithFilterOps.registerCollectionMethods(registry, st)(st.VectorT, WithFilterOps.vector)
      WithFilterOps.registerCollectionWithFilterMethods(registry, st)(st.ListWithFilterT, WithFilterOps.list)
      WithFilterOps.registerCollectionWithFilterMethods(registry, st)(st.VectorWithFilterT, WithFilterOps.vector)

      val staticVector = ReceiverType.Static(Packages.scalettaCollectionVector)
      val vectorApplier = TypeApplier.fromNode(st.VectorT)
      val typeA = Type.variable(0)

      registry.addMultiParamGroupMethod(
        MethodName(staticVector, CommonMethodNames.empty),
        Vector.empty,
        vectorApplier.applyAll(typeA),
        FunctionImpl.objectResult(_ => Vector.empty[Any])
      )

      registry.addMethod(
        MethodName(staticVector, CommonMethodNames.from),
        Vector(FormalParameter(CommonParameterNames.it, TypeApplier.fromNode(st.IterableOnceT).applyAll(typeA))),
        vectorApplier.applyAll(typeA),
        FunctionImpl.objectResult(args => Vector.from(args.read(0).asInstanceOf[IterableOnce[Any]]))
      )
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
