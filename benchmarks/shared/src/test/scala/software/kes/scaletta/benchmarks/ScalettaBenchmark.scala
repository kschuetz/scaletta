package software.kes.scaletta.benchmarks

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

trait ScalettaBenchmark extends AnyFunSuite with Matchers with BeforeAndAfterAll {

  protected def defaultSettings: BenchmarkSettings = BenchmarkSettings.default

  /**
   * Runs the given code as a benchmark.
   *
   * @param name the name of the benchmark
   * @param code the code to measure
   */
  def runBenchmark(name: String,
                   settingsModifiers: BenchmarkSettings => BenchmarkSettings*)
                  (code: => Any): Unit = {
    val settings = settingsModifiers.foldLeft(defaultSettings) {
      case (acc, mod) => mod(acc)
    }

    def runWarmupPhase(): Unit = {
      // Warmup phase
      var i = 0
      while (i < settings.warmupIterations) {
        code
        i += 1
      }
    }

    val results = new Array[Long](settings.measurementIterations)
    var overallTimeNanos = 0L

    def runMeasurementPhase(): Unit = {
      // Measurement phase
      var i = 0
      val t0 = System.nanoTime()
      while (i < settings.measurementIterations) {
        val start = System.nanoTime()
        code
        val end = System.nanoTime()
        results(i) = end - start
        i += 1
      }
      overallTimeNanos = System.nanoTime() - t0
    }

    runWarmupPhase()
    runMeasurementPhase()

    report(name, settings.measurementIterations, overallTimeNanos, results)
  }

  private def report(name: String,
                     iterations: Int,
                     overallTimeNanos: Long,
                     results: Array[Long]): Unit = {
    val avg = overallTimeNanos.toDouble / iterations
    val min = results.min
    val max = results.max
    val sorted = results.sorted
    val p95 = sorted((results.length * 0.95).toInt.min(results.length - 1))

    println(s"Benchmark: $name (${Platform.name})")
    println(s"  Iterations: $iterations")
    println(f"  Overall:    ${overallTimeNanos / 1000000.0}%.4f ms")
    println(f"  Average:    ${avg / 1000000.0}%.4f ms")
    println(f"  Min:        ${min / 1000000.0}%.4f ms")
    println(f"  Max:        ${max / 1000000.0}%.4f ms")
    println(f"  p95:        ${p95 / 1000000.0}%.4f ms")
    println("-" * 40)
  }

}
