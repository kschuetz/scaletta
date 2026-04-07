package software.kes.scaletta.api

import software.kes.scaletta.builtins.{FunctionId, FunctionImpl, ParameterGroup}
import software.kes.scaletta.symbols.{Name, QualifiedName}
import software.kes.scaletta.types.{Type, TypeId}

trait MethodRegistry {
  def addPureMethod(typ: Type.Nominal[TypeId],
                    name: Name,
                    paramGroups: Vector[ParameterGroup],
                    returnType: Type[TypeId],
                    impl: FunctionImpl): FunctionId

  def addPureFunction(name: QualifiedName.Full,
                      paramGroups: Vector[ParameterGroup],
                      returnType: Type[TypeId],
                      impl: FunctionImpl): FunctionId

  def addImpureMethod(typ: Type.Nominal[TypeId],
                      name: Name,
                      paramGroups: Vector[ParameterGroup],
                      returnType: Type[TypeId],
                      impl: FunctionImpl): FunctionId

  def addImpureFunction(name: QualifiedName.Full,
                        paramGroups: Vector[ParameterGroup],
                        returnType: Type[TypeId],
                        impl: FunctionImpl): FunctionId
}
