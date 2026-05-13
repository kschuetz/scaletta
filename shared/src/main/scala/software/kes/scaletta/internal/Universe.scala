package software.kes.scaletta.internal

import software.kes.scaletta.api._
import software.kes.scaletta.internal.builtins.{MethodResolver, MethodUniverse, NativeFunctionDefinition, ResolutionError}
import software.kes.scaletta.internal.symbols.SignatureQuery
import software.kes.scaletta.internal.types.TypeUniverse

object Universe {
  def create(typeUniverse: TypeUniverse,
             methodUniverse: MethodUniverse): Universe =
    new Universe(typeUniverse, methodUniverse)
}

final class Universe private(val typeUniverse: TypeUniverse,
                             val methodUniverse: MethodUniverse) extends MethodResolver {
  def getMethodCandidates(typ: Type.Nominal[TypeId],
                          name: Name,
                          signatureQuery: SignatureQuery): List[NativeFunctionDefinition] = {
    methodUniverse.symbolTable.getMethod(typ, name) match {
      case Some(overloads) => overloads.findCandidates(typeUniverse, signatureQuery)
      case None => List.empty[NativeFunctionDefinition]
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

  def resolveBestMethod(typ: Type.Nominal[TypeId],
                        name: Name,
                        signatureQuery: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition] = {
    methodUniverse.symbolTable.getMethod(typ, name) match {
      case Some(overloads) => overloads.resolveBestMatch(typeUniverse, signatureQuery)
      case None => Left(ResolutionError.NotFound)
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
}
