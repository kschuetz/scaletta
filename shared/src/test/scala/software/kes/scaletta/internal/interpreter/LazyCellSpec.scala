package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.common.BasicTypes

class LazyCellSpec extends AnyFunSpec with Matchers {
  describe("LazyCell") {
    describe("LazyObject") {
      it("should be initially unevaluated") {
        val cell = LazyCell.object_()
        cell.evaluated shouldBe false
        cell.value shouldBe null
      }

      it("should update and push value correctly") {
        val stack = OperandStack.create()
        val cell = LazyCell.object_()
        val value = "test value"
        stack.pushObject(value)

        cell.update(stack)
        cell.evaluated shouldBe true
        cell.value shouldBe value
        stack.size() shouldBe 1

        val stack2 = OperandStack.create()
        cell.pushValue(stack2)
        stack2.pop() shouldBe value
      }
    }

    describe("LazyBoolean") {
      it("should be initially unevaluated") {
        val cell = LazyCell.boolean()
        cell.evaluated shouldBe false
        cell.value shouldBe false
      }

      it("should update and push value correctly") {
        val stack = OperandStack.create()
        val cell = LazyCell.boolean()
        val value = true
        stack.pushBoolean(value)

        cell.update(stack)
        cell.evaluated shouldBe true
        cell.value shouldBe value
        stack.size() shouldBe 1

        val stack2 = OperandStack.create()
        cell.pushValue(stack2)
        stack2.pop() shouldBe value
      }
    }

    describe("LazyInt") {
      it("should be initially unevaluated") {
        val cell = LazyCell.int()
        cell.evaluated shouldBe false
        cell.value shouldBe 0
      }

      it("should update and push value correctly") {
        val stack = OperandStack.create()
        val cell = LazyCell.int()
        val value = 41
        stack.pushInt(value)

        cell.update(stack)
        cell.evaluated shouldBe true
        cell.value shouldBe value
        stack.size() shouldBe 1

        val stack2 = OperandStack.create()
        cell.pushValue(stack2)
        stack2.pop() shouldBe value
      }
    }

    describe("LazyLong") {
      it("should be initially unevaluated") {
        val cell = LazyCell.long()
        cell.evaluated shouldBe false
        cell.value shouldBe 0L
      }

      it("should update and push value correctly") {
        val stack = OperandStack.create()
        val cell = LazyCell.long()
        val value = 43L
        stack.pushLong(value)

        cell.update(stack)
        cell.evaluated shouldBe true
        cell.value shouldBe value
        stack.size() shouldBe 1

        val stack2 = OperandStack.create()
        cell.pushValue(stack2)
        stack2.pop() shouldBe value
      }
    }

    describe("LazyShort") {
      it("should be initially unevaluated") {
        val cell = LazyCell.short()
        cell.evaluated shouldBe false
        cell.value shouldBe 0.toShort
      }

      it("should update and push value correctly") {
        val stack = OperandStack.create()
        val cell = LazyCell.short()
        val value = 41.toShort
        stack.pushShort(value)

        cell.update(stack)
        cell.evaluated shouldBe true
        cell.value shouldBe value
        stack.size() shouldBe 1

        val stack2 = OperandStack.create()
        cell.pushValue(stack2)
        stack2.pop() shouldBe value
      }
    }

    describe("LazyByte") {
      it("should be initially unevaluated") {
        val cell = LazyCell.byte()
        cell.evaluated shouldBe false
        cell.value shouldBe 0.toByte
      }

      it("should update and push value correctly") {
        val stack = OperandStack.create()
        val cell = LazyCell.byte()
        val value = 43.toByte
        stack.pushByte(value)

        cell.update(stack)
        cell.evaluated shouldBe true
        cell.value shouldBe value
        stack.size() shouldBe 1

        val stack2 = OperandStack.create()
        cell.pushValue(stack2)
        stack2.pop() shouldBe value
      }
    }

    describe("LazyChar") {
      it("should be initially unevaluated") {
        val cell = LazyCell.char()
        cell.evaluated shouldBe false
        cell.value shouldBe '\u0000'
      }

      it("should update and push value correctly") {
        val stack = OperandStack.create()
        val cell = LazyCell.char()
        val value = 'A'
        stack.pushChar(value)

        cell.update(stack)
        cell.evaluated shouldBe true
        cell.value shouldBe value
        stack.size() shouldBe 1

        val stack2 = OperandStack.create()
        cell.pushValue(stack2)
        stack2.pop() shouldBe value
      }
    }

    describe("LazyDouble") {
      it("should be initially unevaluated") {
        val cell = LazyCell.double()
        cell.evaluated shouldBe false
        cell.value shouldBe 0.0
      }

      it("should update and push value correctly") {
        val stack = OperandStack.create()
        val cell = LazyCell.double()
        val value = 41.5
        stack.pushDouble(value)

        cell.update(stack)
        cell.evaluated shouldBe true
        cell.value shouldBe value
        stack.size() shouldBe 1

        val stack2 = OperandStack.create()
        cell.pushValue(stack2)
        stack2.pop() shouldBe value
      }
    }

    describe("LazyFloat") {
      it("should be initially unevaluated") {
        val cell = LazyCell.float()
        cell.evaluated shouldBe false
        cell.value shouldBe 0.0f
      }

      it("should update and push value correctly") {
        val stack = OperandStack.create()
        val cell = LazyCell.float()
        val value = 43.5f
        stack.pushFloat(value)

        cell.update(stack)
        cell.evaluated shouldBe true
        cell.value shouldBe value
        stack.size() shouldBe 1

        val stack2 = OperandStack.create()
        cell.pushValue(stack2)
        stack2.pop() shouldBe value
      }
    }

    describe("create") {
      it("should create correct cell types") {
        LazyCell.create(BasicTypes.Boolean) shouldBe a[LazyBoolean]
        LazyCell.create(BasicTypes.Int) shouldBe a[LazyInt]
        LazyCell.create(BasicTypes.Long) shouldBe a[LazyLong]
        LazyCell.create(BasicTypes.Short) shouldBe a[LazyShort]
        LazyCell.create(BasicTypes.Byte) shouldBe a[LazyByte]
        LazyCell.create(BasicTypes.Char) shouldBe a[LazyChar]
        LazyCell.create(BasicTypes.Double) shouldBe a[LazyDouble]
        LazyCell.create(BasicTypes.Float) shouldBe a[LazyFloat]
        LazyCell.create(BasicTypes.Object) shouldBe a[LazyObject]
      }
    }
  }
}
