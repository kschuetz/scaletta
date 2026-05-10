package software.kes.scaletta.internal

import software.kes.scaletta.api.{RuntimeContextId, RuntimeContextRegistry}

private[scaletta] final class RuntimeContextRegistryImpl extends RuntimeContextRegistry {
  private var nextId = 1

  def createRuntimeContextType(): RuntimeContextId = {
    val id = RuntimeContextId(nextId)
    nextId += 1
    id
  }
}
