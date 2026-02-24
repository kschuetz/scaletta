package software.kes.scaletta.testsupport

object LineEndingInterpolators {
  implicit final class LineEndingOps(private val sc: StringContext) extends AnyVal {
    /**
     * Normalizes all line endings to LF.
     */
    def lf(args: Any*): String = {
      normalize(sc.s(args: _*), "\n")
    }

    /**
     * Normalizes all line endings to CR.
     */
    def cr(args: Any*): String = {
      normalize(sc.s(args: _*), "\r")
    }

    /**
     * Normalizes all line endings to CRLF.
     */
    def crlf(args: Any*): String = {
      normalize(sc.s(args: _*), "\r\n")
    }

    private def normalize(input: String, target: String): String = {
      val sb = new StringBuilder
      var lastWasCr = false

      def addTarget(): Unit = {
        sb.append(target)
      }

      val len = input.length
      var i = 0
      while (i < len) {
        val c = input.charAt(i)
        if (c == '\r') {
          addTarget()
          lastWasCr = true
        } else if (c == '\n') {
          if (!lastWasCr) {
            addTarget()
          }
          lastWasCr = false
        } else {
          sb.append(c)
          lastWasCr = false
        }
        i += 1
      }
      sb.toString()
    }
  }
}
