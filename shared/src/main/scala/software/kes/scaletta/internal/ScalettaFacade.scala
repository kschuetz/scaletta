package software.kes.scaletta.internal

import software.kes.scaletta.api.{Scaletta, ScalettaModule, Settings}
import software.kes.scaletta.internal.builtins.{MethodUniverse, MethodUniverseBuilder}
import software.kes.scaletta.internal.library.standard.StandardTypesImpl
import software.kes.scaletta.internal.types.{TypeRegistryImpl, TypeUniverse}

object ScalettaFacade {

  def builder: Builder = {
    val settings = Settings()
    new Builder(settings, Nil)
  }

  final class Builder private[ScalettaFacade](val settings: Settings,
                                              private val modules: List[ScalettaModule[Unit]])
    extends Scaletta.Builder {

    def addModule[A](modulesToAdd: ScalettaModule[A]*): Builder =
      new Builder(settings, modulesToAdd.toList.map(_.unit) ++ modules)

    def modifySettings(fns: Settings => Settings*): Builder = {
      val updatedSettings = fns.foldLeft(settings) {
        case (acc, fn) => fn(acc)
      }
      new Builder(updatedSettings, modules)
    }

    def build: Scaletta = {
      val methodRegistry = MethodUniverseBuilder.create()
      val typeRegistry = new TypeRegistryImpl()
      val runtimeContextRegistry = new RuntimeContextRegistryImpl()

      new StandardTypesImpl(typeRegistry)

      val setup = new SetupImpl(methodRegistry, typeRegistry, runtimeContextRegistry)
      modules.reverse.foreach(_.configure(setup))

      new ScalettaFacade(typeRegistry.build(), methodRegistry.build())
    }
  }

}

final class ScalettaFacade(val typeUniverse: TypeUniverse,
                           val methodUniverse: MethodUniverse) extends Scaletta {

}
