package ch.epfl.ognjanovic.stevan.light

import ch.epfl.ognjanovic.stevan.types.Height
import ch.epfl.ognjanovic.stevan.types.SignedHeader.SignedHeader
import stainless.annotation.pure
import utils.SetInvariants

case class TrustedState(trustedSignedHeader: SignedHeader) {

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
  def increaseTrust(signedHeader: SignedHeader): TrustedState = {
    require(signedHeader.header.height > this.trustedSignedHeader.header.height && trusted(signedHeader))
    TrustedState(signedHeader)
  }

  def isAdjacent(signedHeader: SignedHeader): Boolean =
    signedHeader.header.height == trustedSignedHeader.header.height + 1

  def adjacentHeaderTrust(signedHeader: SignedHeader): Boolean = {
    require(isAdjacent(signedHeader))
    trustedSignedHeader.header.nextValidatorSet == signedHeader.header.validatorSet
  }

  def nonAdjacentHeaderTrust(signedHeader: SignedHeader): Boolean = {
    require(signedHeader.header.height > this.trustedSignedHeader.header.height && !isAdjacent(signedHeader))
    val intersection = SetInvariants.setIntersection(
      trustedSignedHeader.header.nextValidatorSet.keys,
      signedHeader.commit)
    trustedSignedHeader
      .header
      .nextValidatorSet
      .checkSupport(intersection)
  }

  @inline
  def bisectionHeight(signedHeader: SignedHeader): Height = {
    require(signedHeader.header.height > this.trustedSignedHeader.header.height + 1)
    (signedHeader.header.height + trustedSignedHeader.header.height) / 2
  }

  def trusted(signedHeader: SignedHeader): Boolean = {
    require(signedHeader.header.height > currentHeight())
    if (isAdjacent(signedHeader))
      adjacentHeaderTrust(signedHeader)
    else
      nonAdjacentHeaderTrust(signedHeader)
  }
}
