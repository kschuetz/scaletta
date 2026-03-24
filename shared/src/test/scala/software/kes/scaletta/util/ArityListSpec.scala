package software.kes.scaletta.util

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class ArityListSpec extends AnyFunSpec with Matchers {
  describe("ArityList") {
    it("should correctly handle equality") {
      val list1 = ArityList.of(1, 2)
      val list2 = ArityList.of(1, 2)
      val list3 = ArityList.of(1, 3)
      val list4 = ArityList.of(1, 2, 3)

      list1 shouldBe list2
      list1 shouldNot be(list3)
      list1 shouldNot be(list4)
      list1 shouldNot be(EmptyArityList)
      EmptyArityList shouldBe ArityList.empty[Int]
    }

    it("should correctly handle sameArityAs") {
      val list1 = ArityList.of(1, 2, 3)
      val list2 = ArityList.of("a", "b", "c")
      val list3 = ArityList.of(1, 2)
      val empty = ArityList.empty[Int]

      list1.sameArityAs(list2) shouldBe true
      list1.sameArityAs(list3) shouldBe false
      list1.sameArityAs(empty) shouldBe false
      empty.sameArityAs(ArityList.empty[String]) shouldBe true
    }

    it("should correctly handle isEmpty and nonEmpty") {
      ArityList.empty.isEmpty shouldBe true
      ArityList.empty.nonEmpty shouldBe false
      ArityList.of(1).isEmpty shouldBe false
      ArityList.of(1).nonEmpty shouldBe true
    }

    it("should correctly handle tail") {
      val list = NonEmptyArityList.of(1, 2, 3)
      val t1 = list.tail.asInstanceOf[NonEmptyArityList[Int]]
      t1.arity shouldBe 2
      t1.items shouldBe List(2, 3)

      val t2 = t1.tail.asInstanceOf[NonEmptyArityList[Int]]
      t2.arity shouldBe 1
      t2.items shouldBe List(3)

      val t3 = t2.tail
      t3 shouldBe EmptyArityList
    }

    it("should correctly handle head on NonEmptyArityList") {
      val list = NonEmptyArityList.of(41, 43)
      list.head shouldBe 41
      list.tail.asInstanceOf[NonEmptyArityList[Int]].head shouldBe 43
    }

    it("should correctly handle prepend") {
      val list = ArityList.of(2, 3)
      val updated = list.prepend(1)
      updated.arity shouldBe 3
      updated.items shouldBe List(1, 2, 3)

      val fromEmpty = EmptyArityList.prepend(1)
      fromEmpty.arity shouldBe 1
      fromEmpty.items shouldBe List(1)
    }

    it("should correctly handle map") {
      val list = ArityList.of(1, 2, 3)
      val updated = list.map(_ * 2)
      updated.arity shouldBe 3
      updated.items shouldBe List(2, 4, 6)

      EmptyArityList.map((x: Int) => x * 2) shouldBe EmptyArityList
    }

    it("should correctly handle foreach") {
      val list = ArityList.of(1, 2, 3)
      var sum = 0
      list.foreach(sum += _)
      sum shouldBe 6

      var count = 0
      EmptyArityList.foreach((_: Int) => count += 1)
      count shouldBe 0
    }

    it("should correctly handle forall") {
      val list = ArityList.of(1, 2, 3)
      list.forall(_ > 0) shouldBe true
      list.forall(_ > 1) shouldBe false

      EmptyArityList.forall((_: Int) => false) shouldBe true
    }

    it("should correctly handle exists") {
      val list = ArityList.of(1, 2, 3)
      list.exists(_ == 2) shouldBe true
      list.exists(_ == 4) shouldBe false

      EmptyArityList.exists((_: Int) => true) shouldBe false
    }

    describe("factory methods") {
      it("should create from different Iterable types") {
        val fromSet = ArityList.fromSeq(Set(1, 2, 3).toSeq)
        fromSet.arity shouldBe 3
        fromSet.items should contain allOf(1, 2, 3)

        val fromVector = ArityList.fromSeq(Vector(1, 2))
        fromVector.arity shouldBe 2
        fromVector.items shouldBe List(1, 2)
      }

      it("NonEmptyArityList.tryFrom should return None for empty input") {
        NonEmptyArityList.tryFrom(Nil) shouldBe None
        NonEmptyArityList.tryFrom(Set.empty[Int]) shouldBe None
      }

      it("NonEmptyArityList.from should throw on empty input") {
        intercept[RuntimeException] {
          NonEmptyArityList.from(Nil)
        }
      }
    }

    describe("zip") {
      it("should truncate to the shorter list when this is longer") {
        val list1 = ArityList.of(1, 2, 3)
        val list2 = ArityList.of("a", "b")
        val zipped = list1.zip(list2)
        zipped.arity shouldBe 2
        zipped.items shouldBe List((1, "a"), (2, "b"))
      }

      it("should truncate to the shorter list when other is longer") {
        val list1 = ArityList.of(1, 2)
        val list2 = ArityList.of("a", "b", "c")
        val zipped = list1.zip(list2)
        zipped.arity shouldBe 2
        zipped.items shouldBe List((1, "a"), (2, "b"))
      }

      it("should return an empty list when zipped with an empty list") {
        val list1 = ArityList.of(1, 2, 3)
        val list2 = ArityList.empty[String]
        val zipped = list1.zip(list2)
        zipped shouldBe EmptyArityList

        val zippedReverse = list2.zip(list1)
        zippedReverse shouldBe EmptyArityList
      }

      it("should handle nested ArityLists") {
        val nested = ArityList.of(ArityList.of(1, 2), ArityList.of(3, 4))
        nested.arity shouldBe 2
        nested.items.head.arity shouldBe 2
      }
    }

    describe("Pattern Matching") {
      it("should correctly support sequence patterns via ArityList.unapplySeq") {
        val list: ArityList[Int] = ArityList.of(1, 2, 41)

        list match {
          case ArityList(1, 2, 41) => // Success
          case _ => fail("Pattern match failed")
        }

        list match {
          case ArityList(1, _*) => // Success
          case _ => fail("Wildcard pattern match failed")
        }
      }

      it("should correctly support head/tail deconstruction via NonEmptyArityList.unapply") {
        val list: ArityList[Int] = ArityList.of(1, 2, 41)

        list match {
          case NonEmptyArityList(head, tail) =>
            head shouldBe 1
            tail shouldBe ArityList.of(2, 41)
          case EmptyArityList => fail("Should not have matched EmptyArityList")
        }
      }

      it("should correctly match EmptyArityList") {
        val list: ArityList[Int] = EmptyArityList

        list match {
          case NonEmptyArityList(_, _) => fail("Should not have matched NonEmptyArityList")
          case EmptyArityList => // Success
        }
      }
    }
  }
}
