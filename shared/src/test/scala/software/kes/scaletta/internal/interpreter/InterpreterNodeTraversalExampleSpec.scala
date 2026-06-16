package software.kes.scaletta.internal.interpreter

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api._
import software.kes.scaletta.common.BasicTypes
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup
import software.kes.scaletta.internal.runtime.{CoreTypes, FrameSignature, VarSpaceSignature}
import software.kes.scaletta.testsupport.emptyContextReader

class InterpreterNodeTraversalExampleSpec extends AnyFunSuite with Matchers {
  private var nodeLib: NodeLib = _

  /**
   * Module to register Node accessors and capture their IDs.
   */
  private val nodeModule: ScalettaModule[NodeLib] = ScalettaModule { setup =>
    val nodeType = setup.typeRegistry.addRefType(PackagePath.root.qualify(Name("Node")))

    val nextId = setup.methodRegistry.addMethod(
      MethodName(ReceiverType.Instance(nodeType), Name("next")),
      Vector.empty,
      CoreTypes.AnyRefT,
      FunctionImpl.objectResult(args => args.unsafeReadObject(0).asInstanceOf[Node].next)
    )

    val valueId = setup.methodRegistry.addMethod(
      MethodName(ReceiverType.Instance(nodeType), Name("value")),
      Vector.empty,
      CoreTypes.IntT,
      FunctionImpl.intResult(args => args.unsafeReadObject(0).asInstanceOf[Node].value)
    )

    NodeLib(nextId, valueId, nodeType)
  }

  private val scaletta = Scaletta.create(
    Scaletta.addModule(nodeModule.tap(nodeLib = _))
  ).asInstanceOf[ScalettaFacade]

  private val stdLib = StandardLibraryLookup.create(scaletta.universe)

  import stdLib.{arithmetic, equality}

  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable

  private lazy val getNextId = nodeLib.nextId
  private lazy val getValueId = nodeLib.valueId

  test("host-object linked list traversal") {
    // Var 0: current node (Object), Var 1: sum (Int)
    val frame = FrameSignature.fromSeq(Seq(CoreTypes.AnyRefT, CoreTypes.IntT))
    val signature = VarSpaceSignature.of(frame)
    val builder = ProgramBuilder.create(BasicTypes.Int, signature)
    val assembler = builder.mainAssembler()

    val nodeVar = 0
    val sumVar = 1

    val loopStart = assembler.label()
    val loopExit = assembler.label()

    // Initialize sum = 0
    assembler.pushImmediateInt(0)
    assembler.popIntIntoVar(sumVar)

    // loopStart:
    loopStart.bind()

    // Check if current node is null
    assembler.pushObjectFromVar(nodeVar)
    assembler.pushNull()
    assembler.callNative(equality.refEq)
    assembler.branchIf(loopExit)

    // sum = sum + node.value
    assembler.pushIntFromVar(sumVar)
    assembler.pushObjectFromVar(nodeVar)
    assembler.callNative(getValueId)
    assembler.callNative(arithmetic.int.add.int)
    assembler.popIntIntoVar(sumVar)

    // node = node.next
    assembler.pushObjectFromVar(nodeVar)
    assembler.callNative(getNextId)
    assembler.popObjectIntoVar(nodeVar)

    // goto loopStart
    assembler.branch(loopStart)

    // loopExit:
    loopExit.bind()

    // return sum
    assembler.pushIntFromVar(sumVar)
    assembler.emitReturn()

    val program = builder.build()
    val interpreter = Interpreter.create(program, nativeFunctions)

    // Construct a list: Node(41, Node(43, Node(47, null)))
    val listStructure = Node(41, Node(43, Node(47, null.asInstanceOf[Node])))

    val initializer = Initializer { vs =>
      vs.unsafeWriteObject(nodeVar, listStructure)
    }

    val result = interpreter.run(emptyContextReader, initializer)
    result.intValue() shouldBe 131 // 41 + 43 + 47
  }

  /**
   * Host structure for testing object traversal.
   */
  final case class Node(value: Int, next: Node)

  /**
   * Container for captured NativeFunctionIds.
   */
  final case class NodeLib(nextId: NativeFunctionId,
                           valueId: NativeFunctionId,
                           nodeT: Type.Nominal[TypeId])
}
