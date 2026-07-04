package software.kes.scaletta.api

trait CallTarget {
  def setArgument(index: Int, value: Any): Unit
}
