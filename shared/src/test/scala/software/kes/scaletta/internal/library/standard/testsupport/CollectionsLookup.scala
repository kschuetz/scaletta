package software.kes.scaletta.internal.library.standard.testsupport

import software.kes.scaletta.api.Packages.{scalettaCollection, scalettaCollectionVector}
import software.kes.scaletta.api.{Name, NativeFunctionId, Type, TypeId}
import software.kes.scaletta.internal.Universe
import software.kes.scaletta.internal.builtins.MethodResolver
import software.kes.scaletta.internal.library.standard.StandardTypes
import software.kes.scaletta.internal.symbols.{SignatureQuery, SignatureQueryParameter}

final class CollectionsLookup(methodResolver: MethodResolver) {
  private val universe = methodResolver.asInstanceOf[Universe]

  abstract class MethodLookupBase(typeName: Name) {
    protected val typ: Type[TypeId] = universe.typeUniverse.nameIndex
      .get(scalettaCollection.qualify(typeName)) match {
      case Some(c: Type.Constructor[TypeId]) => Type.Nominal(c.name)
      case Some(n: Type.Nominal[TypeId]) => n
      case Some(other) => throw new AssertionError(s"Type $typeName is not a nominal type or constructor: $other")
      case None => throw new AssertionError(s"Could not find type $typeName")
    }

    val map: NativeFunctionId = resolve("map", SignatureQuery.of(SignatureQueryParameter.unknown))
    val flatMap: NativeFunctionId = resolve("flatMap", SignatureQuery.of(SignatureQueryParameter.unknown))
    val withFilter: NativeFunctionId = resolve("withFilter", SignatureQuery.of(SignatureQueryParameter.unknown))

    protected def resolve(name: String, query: SignatureQuery): NativeFunctionId =
      methodResolver.resolveBestMethod(typ, Name(name), query)
        .getOrElse(throw new AssertionError(s"Could not resolve method $name for $typeName"))
        .nativeFunctionId
  }

  abstract class CollectionBase(typeName: Name) extends MethodLookupBase(typeName) {
    val filter: NativeFunctionId = resolve("filter", SignatureQuery.of(SignatureQueryParameter.unknown))
  }

  abstract class CollectionWithFilterBase(typeName: Name) extends MethodLookupBase(typeName)

  object list extends CollectionBase(StandardTypes.names.ListT)

  object vector extends CollectionBase(StandardTypes.names.VectorT) {
    val empty: NativeFunctionId = resolveStatic("empty", SignatureQuery.empty)
    val from: NativeFunctionId = resolveStatic("from", SignatureQuery.of(SignatureQueryParameter.unknown))

    private def resolveStatic(name: String, query: SignatureQuery): NativeFunctionId =
      methodResolver.resolveBestStaticFunction(scalettaCollectionVector.qualify(Name(name)), query)
        .getOrElse(throw new AssertionError(s"Could not resolve static method $name for Vector"))
        .nativeFunctionId
  }

  object listWithFilter extends CollectionWithFilterBase(StandardTypes.names.ListWithFilterT)

  object vectorWithFilter extends CollectionWithFilterBase(StandardTypes.names.VectorWithFilterT)
}
