package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.pure
import utils.SetInvariants

case class TrustedState(trustedSignedHeader: LightBlock) {

  /**
    * The height of the last block that we trust.
    */
  @pure
  def currentHeight(): Height = trustedSignedHeader.header.height

  /**
    * Tries to "improve" the current trusted state with addition of a new signed header of a greater height.
    *
    * @param signedHeader which will be the new trusted header if it can be trusted
    * @return new trusted state or the old one if the trust is not reachable
    */
  @pure
  def increaseTrust(signedHeader: LightBlock): TrustedState = {
    require(signedHeader.header.height > this.trustedSignedHeader.header.height && trusted(signedHeader))
    TrustedState(signedHeader)
  }.ensuring(res => res.currentHeight() > currentHeight() && res.currentHeight() == signedHeader.header.height)

  @pure
  def isAdjacent(signedHeader: LightBlock): Boolean =
    signedHeader.header.height == trustedSignedHeader.header.height + 1

  @pure
  def adjacentHeaderTrust(signedHeader: LightBlock): Boolean = {
    require(isAdjacent(signedHeader))
    internalAdjacentHeaderTrust(signedHeader)
  }.ensuring(res => res == trusted(signedHeader))

  @pure
  private def internalAdjacentHeaderTrust(signedHeader: LightBlock): Boolean = {
    require(isAdjacent(signedHeader))
    trustedSignedHeader.nextValidatorSet == signedHeader.validatorSet
  }

  @pure
  def nonAdjacentHeaderTrust(signedHeader: LightBlock): Boolean = {
    require(signedHeader.header.height > this.trustedSignedHeader.header.height && !isAdjacent(signedHeader))
    internalNonAdjacentHeaderTrust(signedHeader)
  }.ensuring(res => res == trusted(signedHeader))

  @pure
  private def internalNonAdjacentHeaderTrust(signedHeader: LightBlock): Boolean = {
    require(signedHeader.header.height > this.trustedSignedHeader.header.height && !isAdjacent(signedHeader))

    val intersection = SetInvariants.setIntersection(
      trustedSignedHeader.nextValidatorSet.keys,
      signedHeader.commit.signers)

    trustedSignedHeader.nextValidatorSet.checkSupport(intersection)
  }

  @pure
  def bisectionHeight(height: Height): Height = {
    require(height > this.trustedSignedHeader.header.height + 1)
    (height + trustedSignedHeader.header.height) / 2
  }.ensuring(res => res < height && currentHeight() < res)

  @pure
  def trusted(signedHeader: LightBlock): Boolean = {
    require(signedHeader.header.height > currentHeight())
    if (isAdjacent(signedHeader))
      internalAdjacentHeaderTrust(signedHeader)
    else
      internalNonAdjacentHeaderTrust(signedHeader)
  }
}
