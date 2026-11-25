package software.kes.scaletta.api

sealed trait HostPlatformType {
  def isJVM: Boolean

  def isJS: Boolean
}

object HostPlatformType {
  case object JVM extends HostPlatformType {
    def isJVM: Boolean = true

    def isJS: Boolean = false
  }

  case object JS extends HostPlatformType {
    def isJVM: Boolean = false

    def isJS: Boolean = true
  }
}
