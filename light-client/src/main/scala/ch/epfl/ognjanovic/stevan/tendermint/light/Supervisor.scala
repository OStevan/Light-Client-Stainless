package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}

/**
 * Trait encapsulating full light client implementation with verification, fork detection and evidence reporting.
 * In general it does not have to be thread safe.
 */
trait Supervisor {

  /**
   * Tries to verify the current highest `LightBlock` of the blockchain.
   * @return result of verification
   */
  def verifyToHighest(): Either[LightBlock, Supervisor.Error]

  /**
   * Tries to verify the `LightBlock` at a specific height.
   * @param height of the `LightBlock` which should be verified
   * @return result of verification
   */
  def verifyToHeight(height: Height): Either[LightBlock, Supervisor.Error]

  /**
   * Returns a thread safe handle to this supervisor which should be used in a multithreaded environment.
   * @return for processing verification request
   */
  def handle: Handle
}

object Supervisor {
  trait Error
}
