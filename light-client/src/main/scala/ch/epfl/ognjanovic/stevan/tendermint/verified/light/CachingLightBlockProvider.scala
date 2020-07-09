package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock, PeerId}
import com.github.blemale.scaffeine.Cache

sealed class CachingLightBlockProvider(
  private val delegate: LightBlockProvider,
  private val cache: Cache[(PeerId, Height), LightBlock])
    extends LightBlockProvider {

  override def lightBlock(height: Height): LightBlock = {
    cache.get((delegate.peerId, height), _ ⇒ delegate.lightBlock(height))
  }

  override def latestLightBlock: LightBlock = {
    val latestLightBlock = delegate.latestLightBlock
    cache.put((delegate.peerId, latestLightBlock.header.height), latestLightBlock)
    latestLightBlock
  }

  override def currentHeight: Height = latestLightBlock.header.height

  override def chainId: String = delegate.chainId

  override def peerId: PeerId = delegate.peerId
}
