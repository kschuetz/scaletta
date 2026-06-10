package software.kes.scaletta.internal.interpreter

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.internal.ScalettaFacade
import software.kes.scaletta.internal.library.standard.testsupport.StandardLibraryLookup

class InterpreterComplexExampleSpec extends AnyFunSuite with Matchers {
  private val scaletta = Scaletta.create()
    .asInstanceOf[ScalettaFacade]
  private val stdLib = StandardLibraryLookup.create(scaletta.universe)

  import stdLib.arithmetic

  private val nativeFunctions = scaletta.universe.methodUniverse.dispatchTable


}
