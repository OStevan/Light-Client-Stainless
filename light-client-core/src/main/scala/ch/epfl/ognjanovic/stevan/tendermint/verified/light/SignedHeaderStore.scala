package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.{opaque, pure}
import stainless.lang._
import utils.ListMap

object SignedHeaderStore {

  abstract class LightBlockStore {
    @pure
    def put(lightBlock: LightBlock): LightBlockStore = {
      ??? : LightBlockStore
    }.ensuring(res => res.contains(lightBlock.header.height))

    @pure
    def get(height: Height): Option[LightBlock]

    @pure
    def contains(height: Height): Boolean
  }

  case class ListMapLightBlockStore(listMap: ListMap[Height, LightBlock]) extends LightBlockStore {
    @pure
    override def put(lightBlock: LightBlock): LightBlockStore = {
      helperLemma(listMap, (lightBlock.header.height, lightBlock))
      ListMapLightBlockStore(listMap + (lightBlock.header.height, lightBlock))
    }.ensuring(res => res.contains(lightBlock.header.height))

    @pure
    override def get(height: Height): Option[LightBlock] =
      if (contains(height))
        Some(listMap(height))
      else
        None[LightBlock]()

    @pure
    override def contains(height: Height): Boolean = listMap.contains(height)
  }

  @opaque
  private def helperLemma[K, V](listMap: ListMap[K, V], pair: (K, V)): Unit = {
  }.ensuring(_ => (listMap + pair).contains(pair._1))
}