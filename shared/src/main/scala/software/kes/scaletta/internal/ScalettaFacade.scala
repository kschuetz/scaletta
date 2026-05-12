package software.kes.scaletta.internal

import software.kes.scaletta.api._
import software.kes.scaletta.internal.builtins.{MethodUniverse, MethodUniverseBuilder}
import software.kes.scaletta.internal.library.standard.StandardTypesImpl
import software.kes.scaletta.internal.parser.{ParseOptions, Parser}
import software.kes.scaletta.internal.reader.SourceReader
import software.kes.scaletta.internal.reporting.{LineMapBuilder, Pos}
import software.kes.scaletta.internal.scanner.{IdentifierPolicy, Scanner}
import software.kes.scaletta.internal.types.{TypeRegistryImpl, TypeUniverse}
import software.kes.scaletta.util.NonEmptyVector

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

      val standardTypes = new StandardTypesImpl(typeRegistry)

      val setup = new SetupImpl(methodRegistry, typeRegistry, runtimeContextRegistry, standardTypes)
      modules.reverse.foreach(_.configure(setup))

      new ScalettaFacade(typeRegistry.build(), methodRegistry.build(), runtimeContextRegistry)
    }
  }

}

final class ScalettaFacade(val typeUniverse: TypeUniverse,
                           val methodUniverse: MethodUniverse,
                           val runtimeContextRegistry: RuntimeContextRegistry) extends Scaletta {

  def compile(input: String): CompileResult = {
    val lineMapBuilder = LineMapBuilder.create()
    val sourceReader = SourceReader.create(input.iterator, lineMapBuilder)
    val scanner = Scanner.create(sourceReader, IdentifierPolicy.Default)
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
