package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}

import scala.concurrent.Future

/**
 * Thread safe handle to an underlying `Supervisor`.
 */
trait Handle {

  /**
   * Processes a request for highest block verification/
   * @return `Future` containing the result of verification
   */
  def verifyToHighest(): Future[Either[LightBlock, Supervisor.Error]]

  /**
   * Processes a request for verifying a block of specific height.
   * @param height of the block to be verified
   * @return `Future` containing the result of verification
   */
  def verifyToHeight(height: Height): Future[Either[LightBlock, Supervisor.Error]]

  /**
   * Closes this handle. // TODO this should probably be named differently
   */
  def terminate()
}
