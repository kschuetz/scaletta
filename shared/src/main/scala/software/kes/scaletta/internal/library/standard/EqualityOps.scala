package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.FunctionImpl.booleanResult
import software.kes.scaletta.api._
import software.kes.scaletta.internal.runtime.CoreTypes

object EqualityOps {

  lazy val module: ScalettaModule[Unit] =
    ScalettaModule.withPureHint(value = true) {
      ScalettaModule.composite(
        ScalettaModule.methodsOnly(eq.register),
        ScalettaModule.methodsOnly(neq.register),
        ScalettaModule.methodsOnly(refEq.register),
      )
    }

  trait EqualityOp {
    def name: Name

    def operandType: Type.Nominal[TypeId]

    def impl(args: ArgumentReader): Boolean

    def register(registry: MethodRegistry): Unit = {
      registry.addMethod(MethodName(ReceiverType.Instance(operandType), name),
        Vector(FormalParameter(Name("x"), operandType)), CoreTypes.BooleanT,
        booleanResult(impl))
    }
  }

  object eq extends EqualityOp {
    val name: Name = Name("==")

    def operandType: Type.Nominal[TypeId] = CoreTypes.AnyT

    def impl(args: ArgumentReader): Boolean =
      args.read(0) == args.read(1)
  }

  object neq extends EqualityOp {
    val name: Name = Name("!=")

    def operandType: Type.Nominal[TypeId] = CoreTypes.AnyT

    def impl(args: ArgumentReader): Boolean =
      args.read(0) != args.read(1)
  }

  object refEq extends EqualityOp {
    val name: Name = Name("eq")

    def operandType: Type.Nominal[TypeId] = CoreTypes.AnyRefT

    def impl(args: ArgumentReader): Boolean =
      args.unsafeReadObject(0) eq args.unsafeReadObject(1)
  }
}
