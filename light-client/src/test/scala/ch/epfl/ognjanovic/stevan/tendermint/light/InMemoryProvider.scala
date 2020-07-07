package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.light.cases.InputFormat
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock, PeerId}

sealed class InMemoryProvider(override val chainId: String, private val map: Map[Height, LightBlock])
    extends LightBlockProvider {

  override def lightBlock(height: Height): LightBlock = map(height)

  override def currentHeight: Height =
    map.keys.max((x: Height, y: Height) => math.signum((x.value - y.value).toLong).toInt)

  override def latestLightBlock: LightBlock = map(currentHeight)
}

object InMemoryProvider {

  def fromInput(chainId: String, peerId: PeerId, input: Array[InputFormat]): LightBlockProvider = {
    new InMemoryProvider(
      chainId,
      input
        .map(input ⇒
          (
            input.signed_header.header.height,
            LightBlock(
              input.signed_header.header,
              input.signed_header.commit,
              input.validator_set,
              input.next_validator_set,
              peerId)))
        .toMap)
  }

}
