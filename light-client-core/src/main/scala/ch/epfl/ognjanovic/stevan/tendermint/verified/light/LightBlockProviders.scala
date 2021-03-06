package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock, PeerId}
import stainless.annotation._

object LightBlockProviders {

  abstract class LightBlockProvider {

    @pure
    def lightBlock(height: Height): LightBlock = {
      require(height <= currentHeight)
      ??? : LightBlock
    }.ensuring(res => res.header.height == height)

    def latestLightBlock: LightBlock

    def currentHeight: Height

    @pure
    def chainId: String

    def peerId: PeerId

  }

}
