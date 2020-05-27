package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.TrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.pure

case class TrustedState(trustedLightBlock: LightBlock, trustVerifier: TrustVerifier) {

  /**
   * The height of the last block that we trust.
   */
  @pure
  def currentHeight(): Height = trustedLightBlock.header.height

  /**
   * Tries to "improve" the current trusted state with addition of a new signed header of a greater height.
   *
   * @param lightBlock which will be the new trusted header if it can be trusted
   * @return new trusted state or the old one if the trust is not reachable
   */
  @pure
  def increaseTrust(lightBlock: LightBlock): TrustedState = {
    require(lightBlock.header.height > this.trustedLightBlock.header.height && trusted(lightBlock))
    TrustedState(lightBlock, trustVerifier)
  }.ensuring(res => res.currentHeight() > currentHeight() && res.currentHeight() == lightBlock.header.height)

  @pure
  def isAdjacent(lightBlock: LightBlock): Boolean =
    lightBlock.header.height == trustedLightBlock.header.height + 1

  @pure
  def adjacentHeaderTrust(lightBlock: LightBlock): Boolean = {
    require(isAdjacent(lightBlock))
    internalAdjacentHeaderTrust(lightBlock)
  }.ensuring(res => res == trusted(lightBlock))

  @pure
  private def internalAdjacentHeaderTrust(lightBlock: LightBlock): Boolean = {
    require(isAdjacent(lightBlock))
    trustedLightBlock.nextValidatorSet.toInfoHashable == lightBlock.validatorSet.toInfoHashable
  }

  @pure
  def nonAdjacentHeaderTrust(lightBlock: LightBlock): Boolean = {
    require(lightBlock.header.height > this.trustedLightBlock.header.height && !isAdjacent(lightBlock))
    internalNonAdjacentHeaderTrust(lightBlock)
  }.ensuring(res => res == trusted(lightBlock))

  @pure
  private def internalNonAdjacentHeaderTrust(lightBlock: LightBlock): Boolean = {
    require(lightBlock.header.height > this.trustedLightBlock.header.height && !isAdjacent(lightBlock))

    trustVerifier.trustedCommit(trustedLightBlock.nextValidatorSet, lightBlock.commit)
  }

  @pure
  def trusted(lightBlock: LightBlock): Boolean = {
    require(lightBlock.header.height > currentHeight())
    if (isAdjacent(lightBlock))
      internalAdjacentHeaderTrust(lightBlock)
    else
      internalNonAdjacentHeaderTrust(lightBlock)
  }
}
