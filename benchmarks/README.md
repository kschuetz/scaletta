### Benchmarking Progress & Instructions

#### Implementation Summary

- Created `scaletta-benchmarks` project, which is opt-in (not aggregated in root).
- Implemented `ScalettaBenchmark` base trait in
  `benchmarks/shared/src/main/scala/software/kes/scaletta/benchmarks/ScalettaBenchmark.scala`.
    - Integrates JMH with ScalaTest.
    - Allows running benchmarks as standard tests in IntelliJ for easy debugging and profiling.
- Created `InterpreterBenchmark` as a practical sample.

#### How to Run Benchmarks

**1. From IntelliJ (for development/profiling):**

- Navigate to `InterpreterBenchmark.scala`.
- Click the "Run" icon next to the class definition or the `test` method.
- This will execute the `@Benchmark` methods as standard tests.
- **Profiling with JProfiler:** Use the "Profile 'InterpreterBenchmark'" action in IntelliJ. Since it runs as a standard
  test, JProfiler can attach easily without being hindered by JMH forking.

**2. From Command Line (for precision):**

- Use `sbt` to run the JMH benchmarks.
- Example for a single benchmark:
  ```powershell
  sbt "scalettaBenchmarksJVM/jmh:run -f 1 -wi 5 -i 5 benchmarkSimpleAddition"
  ```
- To run all benchmarks in the project:
  ```powershell
  sbt "scalettaBenchmarksJVM/jmh:run"
  ```

#### Build Configuration Details

- The benchmarks project depends on `scaletta`'s `compile` and `test` scopes to allow reusing test support code (like
  `StandardLibraryLookup` and `emptyContextReader`).
- `scalatest` is included as a compile dependency for the benchmarks project to support the hybrid trait.
