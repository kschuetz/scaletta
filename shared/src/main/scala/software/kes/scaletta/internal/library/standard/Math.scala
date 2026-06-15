package software.kes.scaletta.internal.library.standard

import software.kes.scaletta.api.FunctionImpl._
import software.kes.scaletta.api._
import software.kes.scaletta.internal.runtime.CoreTypes._

object Math {
  lazy val module: ScalettaModule[Unit] =
    ScalettaModule.methodsOnly { registry =>
      val staticMath = ReceiverType.Static(Packages.scalettaMath)

      def addUnaryDouble(name: String, fn: Double => Double): Unit = {
        registry.addMethod(
          MethodName(staticMath, Name(name)),
          Vector(FormalParameter.double(Name("x"))),
          DoubleT,
          doubleResult(args => fn(args.unsafeReadDouble(0)))
        )
      }

      // Basic Arithmetic
      addUnaryDouble("sqrt", scala.math.sqrt)
      addUnaryDouble("cbrt", scala.math.cbrt)

      // Trigonometry
      addUnaryDouble("sin", scala.math.sin)
      addUnaryDouble("cos", scala.math.cos)
      addUnaryDouble("tan", scala.math.tan)
      addUnaryDouble("asin", scala.math.asin)
      addUnaryDouble("acos", scala.math.acos)
      addUnaryDouble("atan", scala.math.atan)

      registry.addMethod(
        MethodName(staticMath, Name("atan2")),
        Vector(FormalParameter.double(Name("y")), FormalParameter.double(Name("x"))),
        DoubleT,
        doubleResult(args => scala.math.atan2(args.unsafeReadDouble(0), args.unsafeReadDouble(1)))
      )

      // Power / Log
      registry.addMethod(
        MethodName(staticMath, Name("pow")),
        Vector(FormalParameter.double(Name("x")), FormalParameter.double(Name("y"))),
        DoubleT,
        doubleResult(args => scala.math.pow(args.unsafeReadDouble(0), args.unsafeReadDouble(1)))
      )
      addUnaryDouble("exp", scala.math.exp)
      addUnaryDouble("log", scala.math.log)
      addUnaryDouble("log10", scala.math.log10)

      // Rounding
      addUnaryDouble("ceil", scala.math.ceil)
      addUnaryDouble("floor", scala.math.floor)
      addUnaryDouble("rint", scala.math.rint)

      registry.addMethod(
        MethodName(staticMath, Name("round")),
        Vector(FormalParameter.double(Name("x"))),
        LongT,
        longResult(args => scala.math.round(args.unsafeReadDouble(0)))
      )

      registry.addMethod(
        MethodName(staticMath, Name("round")),
        Vector(FormalParameter.float(Name("x"))),
        IntT,
        intResult(args => scala.math.round(args.unsafeReadFloat(0)))
      )

      // Overloaded methods: abs
      val absReg = registry.overloadRegistryFor(MethodName(staticMath, Name("abs")))
      absReg.addOverload(Vector(FormalParameter.int(Name("x"))), IntT, intResult(args => scala.math.abs(args.unsafeReadInt(0))))
      absReg.addOverload(Vector(FormalParameter.long(Name("x"))), LongT, longResult(args => scala.math.abs(args.unsafeReadLong(0))))
      absReg.addOverload(Vector(FormalParameter.float(Name("x"))), FloatT, floatResult(args => scala.math.abs(args.unsafeReadFloat(0))))
      absReg.addOverload(Vector(FormalParameter.double(Name("x"))), DoubleT, doubleResult(args => scala.math.abs(args.unsafeReadDouble(0))))

      // Overloaded methods: min/max
      List("min", "max").foreach { mName =>
        val reg = registry.overloadRegistryFor(MethodName(staticMath, Name(mName)))
        val fnI: (Int, Int) => Int = if (mName == "min") scala.math.min else scala.math.max
        val fnL: (Long, Long) => Long = if (mName == "min") scala.math.min else scala.math.max
        val fnF: (Float, Float) => Float = if (mName == "min") scala.math.min else scala.math.max
        val fnD: (Double, Double) => Double = if (mName == "min") scala.math.min else scala.math.max

        reg.addOverload(Vector(FormalParameter.int(Name("a")), FormalParameter.int(Name("b"))), IntT, intResult(args => fnI(args.unsafeReadInt(0), args.unsafeReadInt(1))))
        reg.addOverload(Vector(FormalParameter.long(Name("a")), FormalParameter.long(Name("b"))), LongT, longResult(args => fnL(args.unsafeReadLong(0), args.unsafeReadLong(1))))
        reg.addOverload(Vector(FormalParameter.float(Name("a")), FormalParameter.float(Name("b"))), FloatT, floatResult(args => fnF(args.unsafeReadFloat(0), args.unsafeReadFloat(1))))
        reg.addOverload(Vector(FormalParameter.double(Name("a")), FormalParameter.double(Name("b"))), DoubleT, doubleResult(args => fnD(args.unsafeReadDouble(0), args.unsafeReadDouble(1))))
      }

      // Constants (as zero-arg functions)
      registry.addMethod(MethodName(staticMath, Name("E")), Vector.empty, DoubleT, doubleResult(_ => scala.math.E))
      registry.addMethod(MethodName(staticMath, Name("Pi")), Vector.empty, DoubleT, doubleResult(_ => scala.math.Pi))
    }
}
