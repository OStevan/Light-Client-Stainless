package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.pure
import utils.SetInvariants

case class TrustedState(trustedLightBlock: LightBlock) {

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
    TrustedState(lightBlock)
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
    trustedLightBlock.nextValidatorSet == lightBlock.validatorSet
  }

  @pure
  def nonAdjacentHeaderTrust(lightBlock: LightBlock): Boolean = {
    require(lightBlock.header.height > this.trustedLightBlock.header.height && !isAdjacent(lightBlock))
    internalNonAdjacentHeaderTrust(lightBlock)
  }.ensuring(res => res == trusted(lightBlock))

  @pure
  private def internalNonAdjacentHeaderTrust(lightBlock: LightBlock): Boolean = {
    require(lightBlock.header.height > this.trustedLightBlock.header.height && !isAdjacent(lightBlock))

    val intersection = SetInvariants.setIntersection(
      trustedLightBlock.nextValidatorSet.keys,
      lightBlock.commit.forBlock)

    trustedLightBlock.nextValidatorSet.checkSupport(intersection)
  }

  @pure
  def bisectionHeight(height: Height): Height = {
    require(height > this.trustedLightBlock.header.height + 1)
    (height + trustedLightBlock.header.height) / 2
  }.ensuring(res => res < height && currentHeight() < res)

  @pure
  def trusted(lightBlock: LightBlock): Boolean = {
    require(lightBlock.header.height > currentHeight())
    if (isAdjacent(lightBlock))
      internalAdjacentHeaderTrust(lightBlock)
    else
      internalNonAdjacentHeaderTrust(lightBlock)
  }
}
