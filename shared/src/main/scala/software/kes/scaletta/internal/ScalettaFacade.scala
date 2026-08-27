package software.kes.scaletta.internal

import software.kes.scaletta.api._
import software.kes.scaletta.internal.builtins.MethodUniverseBuilder
import software.kes.scaletta.internal.library.standard.{StandardLibrary, StandardTypesImpl}
import software.kes.scaletta.internal.parser.{ParseOptions, Parser}
import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.{LineMapBuilder, Pos}
import software.kes.scaletta.internal.scanner.Scanner
import software.kes.scaletta.internal.types.TypeRegistryImpl
import software.kes.scaletta.util.NonEmptyVector

object ScalettaFacade {

  def builder: Builder = {
    val settings = Settings()
    new Builder(settings, ImportScope.default, Nil)
  }

  final class Builder private[ScalettaFacade](val settings: Settings,
                                              val importScope: ImportScope,
                                              private val modules: List[ScalettaModule[Unit]])
    extends Scaletta.Builder {

    def addModule[A](modulesToAdd: ScalettaModule[A]*): Builder =
      new Builder(settings, importScope, modulesToAdd.toList.map(_.unit) ++ modules)

    def modifySettings(fns: Settings => Settings*): Builder = {
      val updatedSettings = fns.foldLeft(settings) {
        case (acc, fn) => fn(acc)
      }
      new Builder(updatedSettings, importScope, modules)
    }

    def modifyImportScope(fns: ImportScope => ImportScope*): Builder = {
      val updatedImportScope = fns.foldLeft(importScope) {
        case (acc, fn) => fn(acc)
      }
      new Builder(settings, updatedImportScope, modules)
    }

    def build: ScalettaFacade = {
      val methodRegistry = MethodUniverseBuilder.create()
      val typeRegistry = new TypeRegistryImpl()
      val runtimeContextRegistry = new RuntimeContextRegistryImpl()

      val standardTypes = new StandardTypesImpl(typeRegistry)

      val setup = new SetupImpl(methodRegistry, typeRegistry, runtimeContextRegistry, standardTypes)

      StandardLibrary.module.configure(setup)

      modules.reverse.foreach(_.configure(setup))

      val universe = Universe.create(typeRegistry.build(), methodRegistry.build())
      new ScalettaFacade(settings, universe, importScope, runtimeContextRegistry)
    }
  }

}

final class ScalettaFacade(settings: Settings,
                           val universe: Universe,
                           val importScope: ImportScope,
                           val runtimeContextRegistry: RuntimeContextRegistry) extends Scaletta {

  def compile(input: String): CompileResult = {
    val lineMapBuilder = LineMapBuilder.create()
    val sourceReader = SourceReader.create(input.iterator, lineMapBuilder)
    val scanner = Scanner.create(sourceReader, settings.identifierPolicy)
    val parser = Parser.create()
    val parseResult = parser.parse(scanner, ParseOptions(requireExhaustion = true))

    val lineMap = lineMapBuilder.result

    def toDiagnosticMessage(pos: Pos[_], message: String): DiagnosticMessage = {
      val begin = lineMap.indexToPosition(pos.begin)
      val end = lineMap.indexToPosition(pos.end)
      DiagnosticMessage(message, begin, end)
    }

    val errors = parseResult.errors.map(e => toDiagnosticMessage(e, e.value.toString))
    val warnings = parseResult.warnings.map(w => toDiagnosticMessage(w, w.value.toString))
    val hints = parseResult.hints.map(h => toDiagnosticMessage(h, h.value.toString))

    val value: Either[CompileErrors, CompiledExpression] = parseResult.value match {
      case Some(_) if !parseResult.hasErrors =>
        // For now, return a placeholder as the linker/typechecker are not yet integrated
        Right(new CompiledExpression {})
      case _ =>
        val errorList = if (errors.isEmpty) {
          Vector(DiagnosticMessage("Unknown parsing error", Position.first, Position.first))
        } else errors
        Left(CompileErrors(NonEmptyVector(errorList.head, errorList.tail: _*)))
    }

    CompileResult(value, warnings, hints)
  }

}
