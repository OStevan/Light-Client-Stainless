package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.MultiStepVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationTraces.VerificationTrace
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{LightBlock, PeerId}

object ForkDetection {

  trait ForkDetector {

    /**
     * Tries to detect chain forks using several witnesses
     * @param verificationTraceSupplier constructs a verified state for the given PeerId
     * @param targetLightBlock for which we want to check if there are forks
     * @param witnesses list of peers which are used to detect forks
     * @return result of fork detection
     */
    def detectForks(
      verificationTraceSupplier: PeerId ⇒ VerificationTrace,
      targetLightBlock: LightBlock,
      witnesses: List[MultiStepVerifier]): ForkDetectionResult

  }

  // TODO add timeouts
  sealed trait Fork

  case class Forked(primary: LightBlock, witness: LightBlock) extends Fork

  case class Faulty(block: LightBlock, verificationError: VerificationError) extends Fork

  sealed trait ForkDetectionResult

  case object NoForks extends ForkDetectionResult

  case class ForkDetected(detected: List[Fork]) extends ForkDetectionResult
}
