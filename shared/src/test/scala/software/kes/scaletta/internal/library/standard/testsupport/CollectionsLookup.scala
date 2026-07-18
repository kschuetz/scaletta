package software.kes.scaletta.internal.library.standard.testsupport

import software.kes.scaletta.api.Packages.scalettaCollection
import software.kes.scaletta.api.{Name, NativeFunctionId, Type, TypeId}
import software.kes.scaletta.internal.Universe
import software.kes.scaletta.internal.builtins.MethodResolver
import software.kes.scaletta.internal.library.standard.StandardTypes
import software.kes.scaletta.internal.symbols.{SignatureQuery, SignatureQueryParameter}

final class CollectionsLookup(methodResolver: MethodResolver) {
  private val universe = methodResolver.asInstanceOf[Universe]

  abstract class CollectionBase(typeName: Name) {
    private val typ: Type[TypeId] = universe.typeUniverse.nameIndex
      .get(scalettaCollection.qualify(typeName)) match {
      case Some(c: Type.Constructor[TypeId]) => Type.Nominal(c.name)
      case Some(n: Type.Nominal[TypeId]) => n
      case Some(other) => throw new AssertionError(s"Type $typeName is not a nominal type or constructor: $other")
      case None => throw new AssertionError(s"Could not find type $typeName")
    }

    val map: NativeFunctionId = resolve("map", SignatureQuery.of(SignatureQueryParameter.unknown))
    val filter: NativeFunctionId = resolve("filter", SignatureQuery.of(SignatureQueryParameter.unknown))

    private def resolve(name: String, query: SignatureQuery): NativeFunctionId =
      methodResolver.resolveBestMethod(typ, Name(name), query)
        .getOrElse(throw new AssertionError(s"Could not resolve method $name for $typeName"))
        .nativeFunctionId
  }

  object list extends CollectionBase(StandardTypes.names.ListT)

  object vector extends CollectionBase(StandardTypes.names.VectorT)
}
