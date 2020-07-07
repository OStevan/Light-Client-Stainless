package ch.epfl.ognjanovic.stevan.tendermint.light.store

import ch.epfl.ognjanovic.stevan.tendermint.light.LightBlockStatuses.{LightBlockStatus, Trusted, Verified}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}

import scala.collection.mutable

sealed class InMemoryLightStore(
  private val verified: mutable.SortedMap[Height, LightBlock],
  private val trusted: mutable.SortedMap[Height, LightBlock],
  private val inverse: mutable.Map[Height, LightBlockStatus]
) extends LightStore {
  require(
    verified.keys.forall(inverse(_) == Verified) &&
      trusted.keys.forall(inverse(_) == Trusted) &&
      verified.keySet.union(trusted.keySet) == inverse.keySet)

  override def get(height: Height, status: LightBlockStatus): Option[LightBlock] = {
    statusNamespace(status).get(height)
  }

  override def update(lightBlock: LightBlock, status: LightBlockStatus): Unit = {
    inverse
      .get(lightBlock.header.height)
      .foreach(previousStatus ⇒ statusNamespace(previousStatus).remove(lightBlock.header.height))
    statusNamespace(status).put(lightBlock.header.height, lightBlock)
    inverse.put(lightBlock.header.height, status)
  }

  override def remove(height: Height): Unit = {
    inverse.get(height).foreach(status ⇒ statusNamespace(status).remove(height))
  }

  override def latest(status: LightBlockStatus): Option[LightBlock] = {
    val map = statusNamespace(status)
    if (map.isEmpty)
      Option.empty
    else map.get(map.lastKey)
  }

  override def all(status: LightBlockStatus): Iterable[LightBlock] = statusNamespace(status).values

  private def statusNamespace(status: LightBlockStatus): mutable.SortedMap[Height, LightBlock] = {
    status match {
      case Verified ⇒ verified
      case Trusted ⇒ trusted
    }
  }

}

object InMemoryLightStore {

  implicit val heightOrdering: Ordering[Height] = (x: Height, y: Height) => {
    val diff = x.value - y.value
    if (diff < 0) -1 else if (diff == 0) 0 else 1
  }

}
