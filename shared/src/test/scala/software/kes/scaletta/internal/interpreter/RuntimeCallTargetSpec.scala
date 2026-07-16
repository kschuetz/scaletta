package software.kes.scaletta.internal.interpreter

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class RuntimeCallTargetSpec extends AnyFunSuite with Matchers {
  test("creating a runtime call target") {
    val captureSignature = CaptureSignature.empty
    val capturedFrame = new CapturedFrame(captureSignature)
    val closure = new RuntimeClosure(41, capturedFrame)
    val target = new RuntimeCallTarget(closure, 3)

    target.closure shouldBe closure
    target.parameterCount shouldBe 3
  }

  test("setting and getting arguments") {
    val captureSignature = CaptureSignature.empty
    val capturedFrame = new CapturedFrame(captureSignature)
    val closure = new RuntimeClosure(41, capturedFrame)
    val target = new RuntimeCallTarget(closure, 2)

    target.setArgument(0, "first")
    target.setArgument(1, 43)

    target.argumentValues(0) shouldBe "first"
    target.argumentValues(1) shouldBe 43
  }

  test("overwriting an argument") {
    val captureSignature = CaptureSignature.empty
    val capturedFrame = new CapturedFrame(captureSignature)
    val closure = new RuntimeClosure(41, capturedFrame)
    val target = new RuntimeCallTarget(closure, 1)

    target.setArgument(0, "initial")
    target.argumentValues(0) shouldBe "initial"

    target.setArgument(0, "overwritten")
    target.argumentValues(0) shouldBe "overwritten"
  }

  test("rejecting invalid argument indexes") {
    val captureSignature = CaptureSignature.empty
    val capturedFrame = new CapturedFrame(captureSignature)
    val closure = new RuntimeClosure(41, capturedFrame)
    val target = new RuntimeCallTarget(closure, 2)

    assertThrows[IndexOutOfBoundsException] {
      target.setArgument(-1, "invalid")
    }

    assertThrows[IndexOutOfBoundsException] {
      target.setArgument(2, "invalid")
    }

    assertThrows[IndexOutOfBoundsException] {
      // Using apply to test index validation if it was there, but it's an array
      target.argumentValues(-1)
    }

    assertThrows[IndexOutOfBoundsException] {
      target.argumentValues(2)
    }
  }

  test("preserving the wrapped target identity") {
    val captureSignature = CaptureSignature.empty
    val capturedFrame = new CapturedFrame(captureSignature)
    val closure = new RuntimeClosure(41, capturedFrame)
    val target = new RuntimeCallTarget(closure, 0)

    target.closure shouldBe closure
  }
}
