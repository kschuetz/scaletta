package software.kes.scaletta.benchmarks

object BenchmarkSettings {
  val default: BenchmarkSettings = BenchmarkSettings()
}

case class BenchmarkSettings(warmupIterations: Int = 50,
                             measurementIterations: Int = 100) {
  def withWarmupIterations(wi: Int): BenchmarkSettings = copy(warmupIterations = wi)

  def withMeasurementIterations(mi: Int): BenchmarkSettings = copy(measurementIterations = mi)
}
