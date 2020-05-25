package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.rpc.Requester
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}

/**
 * Only does the fetching and of the necessary data using a requester of a specified peer. Doesn't validate hashes, etc.
 */
sealed class DefaultProvider(private val requester: Requester) extends LightBlockProvider {
  /**
   * For a given height gives back the `LightBlock` of that height.
   *
   * @param height of the block, or 0 for the latest block
   * @return block for the specified height
   */
  override def lightBlock(height: Height): LightBlock = {
    val signedHeader = requester.signedHeader(height.value.toLong)
    val validatorSet = requester.validatorSet(height.value.toLong)
    val nextValidatorSet = requester.validatorSet((signedHeader.header.height + 1).value.toLong)
    LightBlock(signedHeader.header, signedHeader.commit, validatorSet, nextValidatorSet, requester.peerId)
  }

  override def currentHeight: Height = requester.signedHeader(0).header.height
}
