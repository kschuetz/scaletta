package software.kes.scaletta.internal.library.standard.testsupport

import software.kes.scaletta.internal.builtins.MethodResolver

object StandardLibraryLookup {
  def create(methodResolver: MethodResolver): StandardLibraryLookup =
    new StandardLibraryLookup(new ArithmeticOpsLookup(methodResolver),
      new CollectionsLookup(methodResolver),
      new ComparisonOpsLookup(methodResolver),
      new DataTypesLookup(methodResolver),
      new EqualityOpsLookup(methodResolver),
      new MathLookup(methodResolver))
}

final class StandardLibraryLookup private(val arithmetic: ArithmeticOpsLookup,
                                          val collections: CollectionsLookup,
                                          val comparison: ComparisonOpsLookup,
                                          val dataTypes: DataTypesLookup,
                                          val equality: EqualityOpsLookup,
                                          val math: MathLookup)
