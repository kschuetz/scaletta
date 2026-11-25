package software.kes.scaletta.api

object HostPlatformInfo extends PlatformHostPlatformInfo {
  def isJVM: Boolean = hostPlatformType.isJVM

  def isJS: Boolean = hostPlatformType.isJS
}
