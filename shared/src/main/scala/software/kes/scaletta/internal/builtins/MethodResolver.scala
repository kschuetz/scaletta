package software.kes.scaletta.internal.builtins

import software.kes.scaletta.api._
import software.kes.scaletta.internal.symbols.SignatureQuery

object MethodResolver {
  val empty: MethodResolver = new MethodResolver {
    def getNativeFunctionDefinition(id: NativeFunctionId): Option[NativeFunctionDefinition] = None

    def getStaticFunctionName(id: NativeFunctionId): Option[QualifiedName.Full] = None

    def getMethodCandidates(typ: Type[TypeId],
                            name: Name,
                            signatureQuery: SignatureQuery): List[NativeFunctionDefinition] = Nil

    def getMethodCandidates(name: Name,
                            signatureQuery: SignatureQuery): List[NativeFunctionDefinition] = Nil

    def getStaticFunctionCandidates(name: QualifiedName,
                                    imports: ImportScope,
                                    signatureQuery: SignatureQuery): List[NativeFunctionDefinition] = Nil

    def getStaticFunctionCandidates(name: QualifiedName.Full,
                                    signatureQuery: SignatureQuery): List[NativeFunctionDefinition] = Nil

    def resolveBestMethod(typ: Type[TypeId],
                          name: Name,
                          signatureQuery: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition] =
      Left(ResolutionError.NotFound)

    def resolveBestStaticFunction(name: QualifiedName,
                                  imports: ImportScope,
                                  signatureQuery: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition] =
      Left(ResolutionError.NotFound)

    def resolveBestStaticFunction(name: QualifiedName.Full,
                                  signatureQuery: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition] =
      Left(ResolutionError.NotFound)
  }
}

trait MethodResolver {
  def getNativeFunctionDefinition(id: NativeFunctionId): Option[NativeFunctionDefinition]

  def getStaticFunctionName(id: NativeFunctionId): Option[QualifiedName.Full]

  def getMethodCandidates(typ: Type[TypeId],
                          name: Name,
                          signatureQuery: SignatureQuery): List[NativeFunctionDefinition]

  def getMethodCandidates(name: Name,
                          signatureQuery: SignatureQuery): List[NativeFunctionDefinition]

  def getStaticFunctionCandidates(name: QualifiedName,
                                  imports: ImportScope,
                                  signatureQuery: SignatureQuery): List[NativeFunctionDefinition]

  def getStaticFunctionCandidates(name: QualifiedName.Full,
                                  signatureQuery: SignatureQuery): List[NativeFunctionDefinition]

  def resolveBestMethod(typ: Type[TypeId],
                        name: Name,
                        signatureQuery: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition]

  def resolveBestStaticFunction(name: QualifiedName,
                                imports: ImportScope,
                                signatureQuery: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition]

  def resolveBestStaticFunction(name: QualifiedName.Full,
                                signatureQuery: SignatureQuery): Either[ResolutionError, NativeFunctionDefinition]
}
