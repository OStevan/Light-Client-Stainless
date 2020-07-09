package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock, PeerId}

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
  sealed trait Error

  case class Io(exception: Exception) extends Error {
    override def toString: String = "Verification experienced an IO exception:\n" + exception.toString
  }

  case class Store(exception: Exception) extends Error {
    override def toString: String = "There was an exception during storing light blocks:\n" + exception.toString
  }

  case object NoPrimary extends Error {
    override def toString: String = "Light client has no more candidates for a primary peer."
  }

  case object NoWitnesses extends Error {
    override def toString: String = "There are no more witnesses to swap with."
  }

  case object NoWitnessLeft extends Error {
    override def toString: String = "There are no more witnesses to use for fork detection."
  }

  case class ForkDetected(peers: Seq[PeerId]) extends Error {
    override def toString: String = "Fork detected for following peers:\n" + peers.toString()
  }

  case object NoValidVerificationState extends Error {
    override def toString: String = "Verification can not proceed with no Trusted or Verified light block."
  }

  case object NoVerifiedState extends Error {
    override def toString: String = "Fork detection can not proceed if there are no trusted light blocks."
  }

  case class TargetLowerThanVerifiedState(targetHeight: Height, trustedHeight: Height) extends Error {

    override def toString: String =
      "Forward verification can not proceed with a target header lower than the current trusted"

  }

  case class VerificationStateOutsideOfTrustingPeriod(lightBlock: LightBlock) extends Error {
    override def toString: String = "Verification failed because of expired state:" + lightBlock.toString
  }

  case class InvalidLightBlock(error: VerificationError) extends Error {
    override def toString: String = "Verification of a light block failed because of: " + error
  }

}
