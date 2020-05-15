package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.rpc.Requester

class DefaultProvider(private val requester: Requester) extends Provider {
  /**
   * For a given height gives back the `LightBlock` of that height.
   *
   * @param height of the block, or 0 for the latest block
   * @return block for the specified height
   */
  override def lightBlock(height: Long): LightBlock = {
    require(height >= 0)
    val signedHeader = requester.signedHeader(height)
    val validatorSet = requester.validatorSet(height)
    val nextValidatorSet = requester.validatorSet(signedHeader.header.height + 1)
    LightBlock(signedHeader, validatorSet, nextValidatorSet, requester.peer)
  }
}
