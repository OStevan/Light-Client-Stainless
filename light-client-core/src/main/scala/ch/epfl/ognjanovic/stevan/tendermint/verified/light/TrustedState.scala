package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.SignedHeaders.SignedHeader
import stainless.annotation.pure
import utils.SetInvariants

case class TrustedState(trustedSignedHeader: SignedHeader) {

  /**
    * The height of the last block that we trust.
    */
  @pure
  def currentHeight(): Height = trustedSignedHeader.header.header.height

  /**
    * Tries to "improve" the current trusted state with addition of a new signed header of a greater height.
    *
    * @param signedHeader which will be the new trusted header if it can be trusted
    * @return new trusted state or the old one if the trust is not reachable
    */
  @pure
  def increaseTrust(signedHeader: SignedHeader): TrustedState = {
    require(signedHeader.header.header.height > this.trustedSignedHeader.header.header.height && trusted(signedHeader))
    TrustedState(signedHeader)
  }.ensuring(res => res.currentHeight() > currentHeight() && res.currentHeight() == signedHeader.header.header.height)

  @pure
  def isAdjacent(signedHeader: SignedHeader): Boolean =
    signedHeader.header.header.height == trustedSignedHeader.header.header.height + 1

  @pure
  def adjacentHeaderTrust(signedHeader: SignedHeader): Boolean = {
    require(isAdjacent(signedHeader))
    internalAdjacentHeaderTrust(signedHeader)
  }.ensuring(res => res == trusted(signedHeader))

  @pure
  private def internalAdjacentHeaderTrust(signedHeader: SignedHeader): Boolean = {
    require(isAdjacent(signedHeader))
    trustedSignedHeader.header.nextValidatorSet == signedHeader.header.validatorSet
  }

  @pure
  def nonAdjacentHeaderTrust(signedHeader: SignedHeader): Boolean = {
    require(signedHeader.header.header.height > this.trustedSignedHeader.header.header.height && !isAdjacent(signedHeader))
    internalNonAdjacentHeaderTrust(signedHeader)
  }.ensuring(res => res == trusted(signedHeader))

  @pure
  private def internalNonAdjacentHeaderTrust(signedHeader: SignedHeader): Boolean = {
    require(signedHeader.header.header.height > this.trustedSignedHeader.header.header.height && !isAdjacent(signedHeader))

    val intersection = SetInvariants.setIntersection(
      trustedSignedHeader.header.nextValidatorSet.keys,
      signedHeader.commit.signers)

    trustedSignedHeader
      .header
      .nextValidatorSet
      .checkSupport(intersection)
  }

  @pure
  def bisectionHeight(height: Height): Height = {
    require(height > this.trustedSignedHeader.header.header.height + 1)
    (height + trustedSignedHeader.header.header.height) / 2
  }.ensuring(res => res < height && currentHeight() < res)

  @pure
  def trusted(signedHeader: SignedHeader): Boolean = {
    require(signedHeader.header.header.height > currentHeight())
    if (isAdjacent(signedHeader))
      internalAdjacentHeaderTrust(signedHeader)
    else
      internalNonAdjacentHeaderTrust(signedHeader)
  }
}
