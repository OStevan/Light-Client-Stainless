package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.MultiStepVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock

object ForkDetection {

  trait ForkDetector {

    /**
     * Tries to detect chain forks using several witnesses
     * @param targetLightBlock for which we want to check if there are forks
     * @param trustedLightBlock is the highest block which has passed both verification and fork detection or was supplied by social consensus
     * @param witnesses list of peers which are used to detect forks
     * @return result of fork detection
     */
    def detectForks(
      targetLightBlock: LightBlock,
      trustedLightBlock: LightBlock,
      witnesses: List[MultiStepVerifier]): ForkDetectionResult

  }

  // TODO add timeouts
  sealed trait Fork

  case class Forked(primary: LightBlock, witness: LightBlock) extends Fork

  // TODO add reason for error
  case class Faulty(block: LightBlock) extends Fork

  sealed trait ForkDetectionResult

  case object NoForks extends ForkDetectionResult

  case class ForkDetected(detected: List[Fork]) extends ForkDetectionResult
}
