package ch.epfl.ognjanovic.stevan.lite

import ch.epfl.ognjanovic.stevan.types.SignedHeader
import stainless.lang._
import stainless.annotation._

/**
 * Abstract class abstracting away main real world and verification concepts, eg. expiration.
 */
abstract class BlockchainClient {
  /**
   * Given the expiration model checks if the block has expired.
   *
   * @param signedHeader to be checked for expiration
   * @return true if expired, false otherwise
   */
  def expired(signedHeader: SignedHeader): Boolean

  @law
  def lawHeightDeterminesExpiration(first: SignedHeader, second: SignedHeader): Boolean = {
    if (first.header.height == second.header.height)
      expired(first) == expired(second)
    else if (first.header.height <= second.header.height)
      expired(second) ==> expired(first)
    else
      expired(first) ==> expired(second)
  }
}