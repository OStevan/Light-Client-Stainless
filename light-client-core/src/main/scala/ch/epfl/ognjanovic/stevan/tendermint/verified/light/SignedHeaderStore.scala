package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.lang._
import utils.ListMap

object SignedHeaderStore {

  abstract class LightBlockStore {
    def put(lightBlock: LightBlock): LightBlockStore = {
      ??? : LightBlockStore
    }.ensuring(res => res.contains(lightBlock.header.height))

    def get(height: Height): Option[LightBlock]

    def contains(height: Height): Boolean
  }

  case class ListMapLightBlockStore(listMap: ListMap[Height, LightBlock]) extends LightBlockStore {
    override def put(lightBLock: LightBlock): LightBlockStore = {
      ListMapLightBlockStore(listMap + (lightBLock.header.height, lightBLock))
    }.ensuring(res => res.contains(lightBLock.header.height))

    override def get(height: Height): Option[LightBlock] =
      if (contains(height))
        Some(listMap(height))
      else
        None[LightBlock]()

    override def contains(height: Height): Boolean = listMap.contains(height)
  }

}