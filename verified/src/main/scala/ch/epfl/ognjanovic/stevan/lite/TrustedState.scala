package ch.epfl.ognjanovic.stevan.lite

import ch.epfl.ognjanovic.stevan.types.SignedHeader
import ch.epfl.ognjanovic.stevan.types.Height

case class TrustedState(trustedSignedHeader: SignedHeader) {

  def currentHeight(): Height = trustedSignedHeader.header.height

  /**
    * Tries "improve" the current trusted state with addition of a new signed header of a greater height.
    *
    * @param signedHeader
    * @return
    */
  def increaseTrust(signedHeader: SignedHeader): TrustedState = {
    require(signedHeader.header.height > this.trustedSignedHeader.header.height)
    if (trusted(signedHeader))
      TrustedState(signedHeader)
    else
      this
  }

  def isAdjacent(signedHeader: SignedHeader): Boolean =
    signedHeader.header.height == trustedSignedHeader.header.height + 1

  def adjacentHeaderTrust(signedHeader: SignedHeader): Boolean = {
    require(isAdjacent(signedHeader))
    trustedSignedHeader.header.nextValidatorSet == signedHeader.header.validatorSet
  }

  def nonAdjacentHeaderTrust(signedHeader: SignedHeader): Boolean = {
    require(signedHeader.header.height > this.trustedSignedHeader.header.height && !isAdjacent(signedHeader))
    trustedSignedHeader
      .header
      .nextValidatorSet
      .checkSupport(trustedSignedHeader.header.nextValidatorSet.keys & signedHeader.commit)
  }

  def bisectionHeight(signedHeader: SignedHeader): Height = {
    require(signedHeader.header.height > this.trustedSignedHeader.header.height + 1)
    (signedHeader.header.height + trustedSignedHeader.header.height) / 2
  }

  private def trusted(signedHeader: SignedHeader): Boolean = {
    require(signedHeader.header.height > trustedSignedHeader.header.height)
    if (isAdjacent(signedHeader))
      adjacentHeaderTrust(signedHeader)
    else
      nonAdjacentHeaderTrust(signedHeader)
  }
}
