package ch.epfl.ognjanovic.stevan.lite

import ch.epfl.ognjanovic.stevan.types.SignedHeader

case class TrustedState(trustedSignedHeader: SignedHeader) {
  /**
    * Tries "improve" the current trusted state with addition of a new signed header of a greater height.
    *
    * @param signedHeader
    * @return
    */
  def increaseTrust(signedHeader: SignedHeader): TrustedState = {
    require(signedHeader.header.height > this.trustedSignedHeader.header.height)
    if (trusted(this.trustedSignedHeader))
      TrustedState(signedHeader)
    else
      this
  }

  /**
    * Given the internal trusted state checks if the untrusted can become trusted,
    * should implement CheckSupport(from TLA).
    *
    * @param trustedHeader
    * @param untrustedHeader 
    * @return true if the untrusted one can become trusted and false otherwise
    */
  def trusted(trustedHeader: SignedHeader): Boolean = ???
}
