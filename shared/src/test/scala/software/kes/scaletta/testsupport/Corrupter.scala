package software.kes.scaletta.testsupport

import org.scalacheck.Gen

object Corrupter {

  /**
   * Randomly corrupts the given source code string by applying one or more mutation strategies.
   *
   * @param source The original, syntactically valid source code.
   * @return A corrupted version of the source code.
   */
  def corrupt(source: String): String = {
    // We split the source into a list of "parts" consisting of tokens (non-whitespace)
    // and whitespace sequences. This allows us to mutate specific parts while
    // maintaining some structural resemblance to the original.
    val tokensAndWhitespace = splitIntoTokensAndWhitespace(source)

    if (tokensAndWhitespace.isEmpty) return ""

    // Apply 1 to 3 mutations to the source
    val corruptionGen = for {
      numMutations <- Gen.choose(1, 3)
      mutatedIndices <- Gen.listOfN(numMutations, Gen.choose(0, tokensAndWhitespace.length - 1))
      mutations <- Gen.listOfN(numMutations, mutationGen)
    } yield {
      val result = tokensAndWhitespace.toBuffer
      mutatedIndices.zip(mutations).foreach { case (idx, mutation) =>
        result(idx) = mutation(result(idx))
      }
      result.mkString
    }

    corruptionGen.sample.getOrElse(source)
  }

  private def splitIntoTokensAndWhitespace(source: String): Vector[String] = {
    val regex = """(\s+|[a-zA-Z_][a-zA-Z0-9_]*|[0-9]+(?:\.[0-9]+)?|[^a-zA-Z0-9_\s])""".r
    regex.findAllIn(source).toVector
  }

  private type Mutation = String => String

  private lazy val mutationGen = Gen.frequency(
    (4, genInjectedJunk),
    (3, genDeletedToken),
    (2, genDuplicatedToken),
    (1, genMisplacedKeyword)
  )

  private def genInjectedJunk: Gen[Mutation] = {
    Gen.oneOf("@", "#", "?", "!", "$", "\\").map { junk =>
      (original: String) => original + junk
    }
  }

  private def genDeletedToken: Gen[Mutation] = {
    Gen.const((_: String) => "")
  }

  private def genDuplicatedToken: Gen[Mutation] = {
    Gen.const((original: String) => original + original)
  }

  private def genMisplacedKeyword: Gen[Mutation] = {
    Gen.oneOf("val", "def", "if", "case", "match", "else", "then", "lazy").map { keyword =>
      (original: String) => {
        if (original.trim.isEmpty) {
          original + keyword + " "
        } else {
          keyword + " " + original
        }
      }
    }
  }
}
