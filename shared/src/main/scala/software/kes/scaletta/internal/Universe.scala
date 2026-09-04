package software.kes.scaletta.internal

import software.kes.scaletta.api._
import software.kes.scaletta.internal.builtins._
import software.kes.scaletta.internal.intermediate.IntermediateExpressionCompiler
import software.kes.scaletta.internal.symbols.SignatureQuery
import software.kes.scaletta.internal.types.TypeUniverse

object Universe {
  def create(typeUniverse: TypeUniverse,
             methodUniverse: MethodUniverse): Universe =
    new Universe(typeUniverse, methodUniverse)
}

final class Universe private(val typeUniverse: TypeUniverse,
                             val methodUniverse: MethodUniverse) extends MethodResolver {

  lazy val compiler: IntermediateExpressionCompiler =
    new IntermediateExpressionCompiler(methodUniverse.dispatchTable)

  def getNativeFunctionDefinition(id: NativeFunctionId): NativeFunctionDefinition =
    methodUniverse.dispatchTable.getDefinition(id)

  def getMethodCandidates(typ: Type[TypeId],
                          name: Name,
                          signatureQuery: SignatureQuery): List[NativeFunctionDefinition] = {
    val overloads = getAllMethodOverloads(typ, name)
    if (overloads.isEmpty) {
      List.empty[NativeFunctionDefinition]
    } else {
      val updatedQuery = updateQueryWithReceiver(typ, signatureQuery)
      OverloadTable(overloads).findCandidates(typeUniverse, updatedQuery)
    }
  }

  def getStaticFunctionCandidates(name: QualifiedName,
                                  imports: ImportScope,
                                  signatureQuery: SignatureQuery): List[NativeFunctionDefinition] = {
    methodUniverse.symbolTable.resolveStaticFunction(name, imports).flatMap { entry =>
      entry.value.findCandidates(typeUniverse, signatureQuery)
    }
  }

  def getStaticFunctionCandidates(name: QualifiedName.Full,
                                  signatureQuery: SignatureQuery): List[NativeFunctionDefinition] = {
    methodUniverse.symbolTable.getStaticFunction(name) match {
      case Some(overloads) => overloads.findCandidates(typeUniverse, signatureQuery)
      case None => List.empty[NativeFunctionDefinition]
    }
  }

  def resolveBestMethod(typ: Type[TypeId],
                        name: Name,
                        signatureQuery: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition] = {
    val overloads = getAllMethodOverloads(typ, name)
    if (overloads.isEmpty) {
      Left(ResolutionError.NotFound)
    } else {
      val updatedQuery = updateQueryWithReceiver(typ, signatureQuery)
      OverloadTable(overloads).resolveBestMatch(typeUniverse, updatedQuery)
    }
  }

  def resolveBestStaticFunction(name: QualifiedName,
                                imports: ImportScope,
                                signatureQuery: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition] = {
    val candidates = getStaticFunctionCandidates(name, imports, signatureQuery)
    candidates match {
      case Nil => Left(ResolutionError.NotFound)
      case best :: Nil => Right(best)
      case _ => Left(ResolutionError.Ambiguous)
    }
  }

  def resolveBestStaticFunction(name: QualifiedName.Full,
                                signatureQuery: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition] = {
    methodUniverse.symbolTable.getStaticFunction(name) match {
      case Some(overloads) => overloads.resolveBestMatch(typeUniverse, signatureQuery)
      case None => Left(ResolutionError.NotFound)
    }
  }

  private def getAllMethodOverloads(typ: Type[TypeId], name: Name): List[NativeFunctionDefinition] = {
    typeUniverse.hierarchy.allAncestors(typ).toList.flatMap { ancestor =>
      methodUniverse.symbolTable.getMethod(ancestor, name) match {
        case Some(overloads) => overloads.variations
        case None => Nil
      }
    }
  }

  private def updateQueryWithReceiver(typ: Type[TypeId], signatureQuery: SignatureQuery): SignatureQuery = {
    signatureQuery.groups match {
      case groups if groups.isEmpty => SignatureQuery.of(typ)
      case groups =>
        val firstGroup = groups(0)
        val updatedFirstGroup = SignatureQuery.Group(typ +: firstGroup.parameters)
        SignatureQuery.ofGroups(updatedFirstGroup +: groups.tail: _*)
    }
  }
}
