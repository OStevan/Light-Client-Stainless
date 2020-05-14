package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain.BlockchainStates.BlockchainState
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.SignedHeader.SignedHeader
import stainless.annotation.extern

case class SoundSignedHeaderProvider(blockchainState: BlockchainState) {
  @extern
  def getSignedHeader(height: Height): SignedHeader = {
    require(height < blockchainState.currentHeight())
    blockchainState.signedHeader(height)
  }.ensuring(res =>
    (res.header == blockchainState.header(height) && res.header.height == height) ||
      (res.commit.subsetOf(blockchainState.faulty) && res.header.height == height))
}
