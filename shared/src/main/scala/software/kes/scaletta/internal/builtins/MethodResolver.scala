package software.kes.scaletta.internal.builtins

import software.kes.scaletta.api._
import software.kes.scaletta.internal.symbols.SignatureQuery

trait MethodResolver {
  def getMethodCandidates(typ: Type[TypeId],
                          name: Name,
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
