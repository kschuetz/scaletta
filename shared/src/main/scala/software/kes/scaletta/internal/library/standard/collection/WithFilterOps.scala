package software.kes.scaletta.internal.library.standard.collection

import software.kes.scaletta.api._
import software.kes.scaletta.internal.library.standard.{CommonMethodNames, CommonParameterNames, StandardTypes}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object WithFilterOps {

  def registerCollectionMethods(registry: MethodRegistry,
                                standardTypes: StandardTypes)
                               (collectionConstructor: Type.Constructor[TypeId],
                                strategy: CollectionStrategy): Unit = {
    val typeA = Type.variable(0)

    val receiverType = ReceiverType.instance(Type.Nominal(collectionConstructor.name))
    val tc = TypeApplier.fromNode(collectionConstructor)

    registry.addMethod(
      MethodName(receiverType, CommonMethodNames.withFilter),
      Vector(FormalParameter(CommonParameterNames.p, Type.function[TypeId](typeA)(standardTypes.BooleanT))),
      tc.applyAll(typeA),
      FunctionImpl.objectResult(strategy.withFilterImpl)
    )
  }

  def registerCollectionWithFilterMethods(registry: MethodRegistry,
                                          standardTypes: StandardTypes)
                                         (collectionWithFilterConstructor: Type.Constructor[TypeId],
                                          strategy: CollectionStrategy): Unit = {
    val typeA = Type.variable(0)
    val typeB = Type.variable(1)

    val receiverType = ReceiverType.instance(Type.Nominal(collectionWithFilterConstructor.name))
    val tc = TypeApplier.fromNode(collectionWithFilterConstructor)

    val flatMapLambdaReturnType = TypeApplier.fromNode(standardTypes.IterableOnceT).applyAll(typeB)
    // flatMap[B](f: A => IterableOnce[B]): C[B]
    registry.addMethod(
      MethodName(receiverType, CommonMethodNames.flatMap),
      Vector(FormalParameter(CommonParameterNames.f, Type.function[TypeId](typeA)(flatMapLambdaReturnType))),
      tc.applyAll(typeB),
      FunctionImpl.higherOrder(strategy.flatMapImpl)
    )

    // map[B](f: A => B): C[B]
    registry.addMethod(
      MethodName(receiverType, CommonMethodNames.map),
      Vector(FormalParameter(CommonParameterNames.f, Type.function[TypeId](typeA)(typeB))),
      tc.applyAll(typeB),
      FunctionImpl.higherOrder(strategy.mapImpl)
    )

    // withFilter(p: A => Boolean): WithFilter[A]
    registry.addMethod(
      MethodName(receiverType, CommonMethodNames.withFilter),
      Vector(FormalParameter(CommonParameterNames.p, Type.function[TypeId](typeA)(standardTypes.BooleanT))),
      tc.applyAll(typeA),
      FunctionImpl.objectResult(strategy.withFilterWithFilterImpl)
    )
  }

  abstract class CollectionStrategy {
    type CollectionWithFilter <: AnyRef

    type Collection <: IterableOnce[_]

    type Builder

    protected def getUnderlying(c: CollectionWithFilter): Collection

    protected def getPredicates(c: CollectionWithFilter): Vector[CallTarget]

    protected def addPredicate(c: CollectionWithFilter,
                               predicate: CallTarget): CollectionWithFilter

    protected def createCollectionWithFilter(c: Collection,
                                             predicate: CallTarget): CollectionWithFilter

    protected def newBuilder(): Builder

    protected def addElement(builder: Builder, elem: Any): Unit

    protected def done(builder: Builder): NativeStep

    lazy val mapImpl: ArgumentReader => NativeStep =
      createMapFunction((builder, elem) => addElement(builder, elem))

    lazy val flatMapImpl: ArgumentReader => NativeStep =
      createMapFunction { (builder, elem) =>
        elem.asInstanceOf[IterableOnce[Any]].iterator.foreach { elem =>
          addElement(builder, elem)
        }
      }

    def withFilterImpl(argumentReader: ArgumentReader): AnyRef = {
      val input = argumentReader.readObject(0).asInstanceOf[Collection]
      val predicate = argumentReader.unsafeReadFunction(1)
      createCollectionWithFilter(input, predicate)
    }

    def withFilterWithFilterImpl(argumentReader: ArgumentReader): AnyRef = {
      val input = argumentReader.readObject(0).asInstanceOf[CollectionWithFilter]
      val predicate = argumentReader.unsafeReadFunction(1)
      addPredicate(input, predicate)
    }

    private def createMapFunction(handleFunctionResult: (Builder, Any) => Unit): ArgumentReader => NativeStep = { argumentReader =>
      val input = argumentReader.readObject(0).asInstanceOf[CollectionWithFilter]
      val underlying = getUnderlying(input)

      val iter = underlying.iterator
      if (iter.isEmpty) NativeStep.done(underlying)
      else {
        val predicates = getPredicates(input)
        val fn = argumentReader.unsafeReadFunction(1)
        val out = newBuilder()

        def go(predicateIndex: Int, elem: Any): NativeStep = {
          if (predicateIndex < predicates.length) {
            val p = predicates(predicateIndex)
            p.setArgument(0, elem)

            NativeStep.call(p, keep => {
              if (keep.asInstanceOf[Boolean]) {
                go(predicateIndex + 1, elem)
              } else if (iter.hasNext) {
                go(0, iter.next())
              } else {
                done(out)
              }
            })
          } else {
            fn.setArgument(0, elem)
            NativeStep.call(fn, mapped => {
              handleFunctionResult(out, mapped)
              if (iter.hasNext) {
                go(0, iter.next())
              } else {
                done(out)
              }
            })
          }
        }

        go(0, iter.next())
      }
    }
  }

  object list extends CollectionStrategy {
    type CollectionWithFilter = ListWithFilter
    type Collection = List[Any]
    type Builder = ListBuffer[Any]

    protected def getUnderlying(c: ListWithFilter): List[Any] =
      c.underlying

    protected def getPredicates(c: ListWithFilter): Vector[CallTarget] =
      c.predicates

    protected def addPredicate(c: ListWithFilter, predicate: CallTarget): ListWithFilter =
      c.copy(predicates = c.predicates :+ predicate)

    protected def createCollectionWithFilter(c: List[Any], predicate: CallTarget): ListWithFilter =
      ListWithFilter(c, Vector(predicate))

    protected def newBuilder(): ListBuffer[Any] =
      new ListBuffer[Any]

    protected def addElement(builder: ListBuffer[Any], elem: Any): Unit =
      builder.addOne(elem)

    protected def done(builder: ListBuffer[Any]): NativeStep =
      NativeStep.done(builder.result())
  }

  object vector extends CollectionStrategy {
    type CollectionWithFilter = VectorWithFilter
    type Collection = Vector[Any]
    type Builder = mutable.ReusableBuilder[Any, Vector[Any]]

    protected def getUnderlying(c: VectorWithFilter): Vector[Any] =
      c.underlying

    protected def getPredicates(c: VectorWithFilter): Vector[CallTarget] =
      c.predicates

    protected def addPredicate(c: VectorWithFilter, predicate: CallTarget): VectorWithFilter =
      c.copy(predicates = c.predicates :+ predicate)

    protected def createCollectionWithFilter(c: Vector[Any], predicate: CallTarget): VectorWithFilter =
      VectorWithFilter(c, Vector(predicate))

    protected def newBuilder(): mutable.ReusableBuilder[Any, Vector[Any]] =
      Vector.newBuilder[Any]

    protected def addElement(builder: mutable.ReusableBuilder[Any, Vector[Any]], elem: Any): Unit =
      builder += elem

    protected def done(builder: mutable.ReusableBuilder[Any, Vector[Any]]): NativeStep =
      NativeStep.done(builder.result())
  }

}
