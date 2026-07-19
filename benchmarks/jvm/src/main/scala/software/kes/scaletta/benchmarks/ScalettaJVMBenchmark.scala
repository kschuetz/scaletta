package software.kes.scaletta.benchmarks

import org.openjdk.jmh.annotations._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.TimeUnit

@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
trait ScalettaJVMBenchmark extends AnyFunSuite with Matchers {

  /**
   * This allows the benchmark to be run as a ScalaTest suite in IntelliJ.
   */
  test("run benchmark as test") {
    runAsTest()
  }

  /**
   * Hook for running the benchmark logic as a test.
   * By default, it finds all methods annotated with @Benchmark and runs them.
   */
  def runAsTest(): Unit = {
    val benchmarkMethods = this.getClass.getMethods.filter(_.isAnnotationPresent(classOf[Benchmark]))
    if (benchmarkMethods.isEmpty) {
      fail("No @Benchmark methods found in " + this.getClass.getSimpleName)
    }

    val setupMethods = this.getClass.getMethods.filter(_.isAnnotationPresent(classOf[Setup]))
    val tearDownMethods = this.getClass.getMethods.filter(_.isAnnotationPresent(classOf[TearDown]))

    setupMethods.foreach(_.invoke(this))
    try {
      benchmarkMethods.foreach { method =>
        println(s"Running benchmark as test: ${method.getName}")
        method.invoke(this)
      }
    } finally {
      tearDownMethods.foreach(_.invoke(this))
    }
  }

}
