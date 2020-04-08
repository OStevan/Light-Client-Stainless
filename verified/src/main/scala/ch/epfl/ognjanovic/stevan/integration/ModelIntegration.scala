package ch.epfl.ognjanovic.stevan.integration

import ch.epfl.ognjanovic.stevan.blockchain.BlockchainStates.BlockchainState
import ch.epfl.ognjanovic.stevan.types.Height
import stainless.lang._
import stainless.annotation._
import ch.epfl.ognjanovic.stevan.types.SignedHeader

object ModelIntegration {
  def snapshotExecution(blockchainState: BlockchainState, trustedHeight: Height, heightToVerify: Height) = {
      require(blockchainState.currentHeight() >= heightToVerify && trustedHeight < heightToVerify)
  }

  case class SoundSignedHeaderProvider(blockchainState: BlockchainState) {
    @extern
    def getSignedHeader(height: Height): SignedHeader = {
      require(height < blockchainState.currentHeight())
      blockchainState.signedHeader(height)
    }.ensuring(res =>
      (res.header == blockchainState.header(height)) ||
      (res.commit.subsetOf(blockchainState.faulty) && res.header.height == height))
  }
}
