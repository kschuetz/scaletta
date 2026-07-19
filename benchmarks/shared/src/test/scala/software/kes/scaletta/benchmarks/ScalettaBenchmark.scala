package software.kes.scaletta.benchmarks

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

trait ScalettaBenchmark extends AnyFunSuite with Matchers with BeforeAndAfterAll {

  /**
   * Number of iterations to run before measurement.
   */
  def warmupIterations: Int = 5

  /**
   * Number of iterations to measure.
   */
  def measurementIterations: Int = 10

  /**
   * Runs the given code as a benchmark.
   *
   * @param name the name of the benchmark
   * @param code the code to measure
   */
  def runBenchmark(name: String)(code: => Any): Unit = {
    def runWarmupPhase(): Unit = {
      // Warmup phase
      var i = 0
      while (i < warmupIterations) {
        code
        i += 1
      }
    }

    val results = new Array[Long](measurementIterations)

    def runMeasurementPhase(): Unit = {
      // Measurement phase
      var i = 0
      while (i < measurementIterations) {
        val start = System.nanoTime()
        code
        val end = System.nanoTime()
        results(i) = end - start
        i += 1
      }
    }

    runWarmupPhase()
    runMeasurementPhase()

    report(name, results)
  }

  private def report(name: String, results: Array[Long]): Unit = {
    val avg = results.sum.toDouble / results.length
    val min = results.min
    val max = results.max
    val sorted = results.sorted
    val p95 = sorted((results.length * 0.95).toInt.min(results.length - 1))

    println(s"Benchmark: $name (${Platform.name})")
    println(s"  Iterations: $measurementIterations")
    println(f"  Average:    ${avg / 1000000.0}%.4f ms")
    println(f"  Min:        ${min / 1000000.0}%.4f ms")
    println(f"  Max:        ${max / 1000000.0}%.4f ms")
    println(f"  p95:        ${p95 / 1000000.0}%.4f ms")
    println("-" * 40)
  }

}
