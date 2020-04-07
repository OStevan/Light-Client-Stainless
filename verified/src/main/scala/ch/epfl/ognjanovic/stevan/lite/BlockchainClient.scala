package ch.epfl.ognjanovic.stevan.lite

import ch.epfl.ognjanovic.stevan.types.{Height, SignedHeader}
import stainless.lang._
import stainless.annotation._

/**
 * Abstract class abstracting away main real world and verification concepts, eg. expiration.
 */
abstract class BlockchainClient {
  /**
   * Send an event to retrive the header for the specified height.
   * 
   * @param height of the header we are interested in
   */
  def requestHeader(height: Height): Unit

  /**
   * Given the expiration model checks if the block has expired.
   *
   * @param signedHeader to be checked for expiration
   * @return true if expired, false otherwise
   */
  def expired(signedHeader: SignedHeader): Boolean
}