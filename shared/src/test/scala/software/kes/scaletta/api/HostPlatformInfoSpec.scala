package software.kes.scaletta.api

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

final class HostPlatformInfoSpec extends AnyFunSuite with Matchers {

  test("HostPlatformInfo correctly identifies runtime platform") {
    if (HostPlatformInfo.isJVM) {
      HostPlatformInfo.hostPlatformType shouldBe HostPlatformType.JVM
      HostPlatformInfo.isJVM shouldBe true
      HostPlatformInfo.isJS shouldBe false
    } else if (HostPlatformInfo.isJS) {
      HostPlatformInfo.hostPlatformType shouldBe HostPlatformType.JS
      HostPlatformInfo.isJS shouldBe true
      HostPlatformInfo.isJVM shouldBe false
    } else {
      fail("HostPlatformInfo reported an unknown runtime platform")
    }
  }

  test("HostPlatformInfo platform flags are mutually exclusive") {
    (HostPlatformInfo.isJVM ^ HostPlatformInfo.isJS) shouldBe true
  }

  test("HostPlatformType constants have correct property values") {
    HostPlatformType.JVM.isJVM shouldBe true
    HostPlatformType.JVM.isJS shouldBe false

    HostPlatformType.JS.isJS shouldBe true
    HostPlatformType.JS.isJVM shouldBe false
  }

  test("numeric type discrimination reflects platform semantics") {
    val value: Any = 43.0
    if (HostPlatformInfo.isJVM) {
      value.isInstanceOf[Int] shouldBe false
    } else {
      value.isInstanceOf[Int] shouldBe true
    }
  }

  test("reference equality of boxed primitives demonstrates platform difference") {
    val a: Any = 1000003
    val b: Any = 1000003
    val areIdentical = a.asInstanceOf[AnyRef] eq b.asInstanceOf[AnyRef]

    if (HostPlatformInfo.isJVM) {
      areIdentical shouldBe false
    } else {
      areIdentical shouldBe true
    }
  }
}
