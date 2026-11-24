package software.kes.scaletta.internal.interpolators

import software.kes.scaletta.api.{CustomInterpolator, InterpolatorId, InterpolatorRegistry, Name}

import scala.collection.mutable

private[scaletta] object InterpolatorRegistryImpl {
  def create(): InterpolatorRegistryImpl = new InterpolatorRegistryImpl()
}

private[scaletta] final class InterpolatorRegistryImpl extends InterpolatorRegistry
  with InterpolatorNameLookup
  with CustomInterpolatorLookup {

  private val nameToId = mutable.Map[Name, InterpolatorId]()
  private val idToInterpolator = mutable.Map[InterpolatorId, CustomInterpolator]()
  private var nextId: Int = 0

  def registerInterpolator(name: Name, interpolator: CustomInterpolator): Unit = {
    val id = nameToId.get(name) match {
      case Some(existingId) => existingId
      case None =>
        val newId = InterpolatorId(nextId)
        nextId += 1
        nameToId.put(name, newId)
        newId
    }
    idToInterpolator.put(id, interpolator)
    ()
  }

  def getInterpolatorByName(name: Name): Option[InterpolatorId] =
    nameToId.get(name)

  def getCustomInterpolator(id: InterpolatorId): CustomInterpolator =
    idToInterpolator.get(id) match {
      case Some(interpolator) => interpolator
      case None => throw new NoSuchElementException(s"Unknown interpolator id: $id")
    }

  def getCustomInterpolator(name: String): Option[LinkedInterpolator.Custom] =
    nameToId.get(Name(name)) match {
      case Some(id) =>
        idToInterpolator.get(id) match {
          case Some(interpolator) => Some(LinkedInterpolator.Custom(id, interpolator.returnType))
          case None => None
        }
      case None => None
    }

  def build(): InterpolatorLookup =
    new InterpolatorLookup(nameToId.toMap, idToInterpolator.toMap)
}

private[scaletta] final class InterpolatorLookup(val nameToId: Map[Name, InterpolatorId],
                                                 val idToInterpolator: Map[InterpolatorId, CustomInterpolator])
  extends InterpolatorNameLookup
    with CustomInterpolatorLookup {

  def getInterpolatorByName(name: Name): Option[InterpolatorId] =
    nameToId.get(name)

  def getCustomInterpolator(id: InterpolatorId): CustomInterpolator =
    idToInterpolator.get(id) match {
      case Some(interpolator) => interpolator
      case None => throw new NoSuchElementException(s"Unknown interpolator id: $id")
    }

  def getCustomInterpolator(name: String): Option[LinkedInterpolator.Custom] =
    nameToId.get(Name(name)) match {
      case Some(id) =>
        idToInterpolator.get(id) match {
          case Some(interpolator) => Some(LinkedInterpolator.Custom(id, interpolator.returnType))
          case None => None
        }
      case None => None
    }
}
