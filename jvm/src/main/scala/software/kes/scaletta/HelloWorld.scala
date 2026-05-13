package software.kes.scaletta

import software.kes.scaletta.api.Scaletta
import software.kes.scaletta.internal.ScalettaFacade

object HelloWorld {
  def main(args: Array[String]): Unit = {
    val x = 123.toShort + 12.toShort

    val y = 'c' + 'd'

    val ch = 'a'

    println(s"Hello world")

    val scaletta = Scaletta.create()
      .asInstanceOf[ScalettaFacade]

    println(scaletta.universe.methodUniverse.dispatchTable.size)
  }
}
