package software.kes.scaletta.types

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.symbols.{ImportScope, QualifiedName}

class TypeNameIndexSpec extends AnyFunSpec with Matchers {
  describe("TypeNameIndex") {
    it("should be empty initially") {
      val index = TypeNameIndex.empty
      index.size shouldBe 0
      index.allNames shouldBe empty
    }

    it("should intern new names and assign incremental TypeIds") {
      val index0 = TypeNameIndex.empty
      val nameA = QualifiedName.full("scaletta.lang.Int")
      val nameB = QualifiedName.full("scaletta.lang.String")

      val (index1, idA) = index0.intern(nameA)
      val (index2, idB) = index1.intern(nameB)

      idA.value shouldBe 0
      idB.value shouldBe 1
      index2.size shouldBe 2
      index2.getName(idA) shouldBe nameA
      index2.getName(idB) shouldBe nameB
    }

    it("should be idempotent when interning the same name") {
      val index0 = TypeNameIndex.empty
      val name = QualifiedName.full("scaletta.lang.Int")

      val (index1, id1) = index0.intern(name)
      val (index2, id2) = index1.intern(name)

      id1 shouldBe id2
      index1 shouldBe index2
      index2.size shouldBe 1
    }

    it("should support direct lookup via get") {
      val name = QualifiedName.full("scaletta.lang.Int")
      val (index, id) = TypeNameIndex.empty.intern(name)

      index.get(name) shouldBe Some(id)
      index.get(QualifiedName.full("other.Type")) shouldBe None
    }

    it("should support resolution via resolve with imports") {
      val nameInt = QualifiedName.full("scaletta.lang.Int")
      val (index, idInt) = TypeNameIndex.empty.intern(nameInt)

      // Currently ImportScope is a stub, so we test with empty and full name resolution
      val results = index.resolve(nameInt, ImportScope.empty)

      results should have size 1
      results.head.value shouldBe idInt
      results.head.name shouldBe "Int"
      results.head.qualifier shouldBe nameInt.qualifier
    }

    it("should correctly retrieve name by TypeId") {
      val nameA = QualifiedName.full("A")
      val nameB = QualifiedName.full("B")
      val (index1, idA) = TypeNameIndex.empty.intern(nameA)
      val (index2, idB) = index1.intern(nameB)

      index2.getName(idA) shouldBe nameA
      index2.getName(idB) shouldBe nameB
    }

    describe("addUnique") {
      it("should add a name that doesn't exist") {
        val name = QualifiedName.full("scaletta.lang.Int")
        val maybeResult = TypeNameIndex.empty.addUnique(name)

        maybeResult shouldBe defined
        val (index, id) = maybeResult.get
        index.size shouldBe 1
        id.value shouldBe 0
        index.get(name) shouldBe Some(id)
      }

      it("should return None when adding a name that already exists") {
        val name = QualifiedName.full("scaletta.lang.Int")
        val (index1, _) = TypeNameIndex.empty.intern(name)

        index1.addUnique(name) shouldBe None
      }
    }

    it("should throw an exception when getName is called with an invalid TypeId") {
      val index = TypeNameIndex.empty
      intercept[IndexOutOfBoundsException] {
        index.getName(TypeId(41))
      }
    }
  }
}
