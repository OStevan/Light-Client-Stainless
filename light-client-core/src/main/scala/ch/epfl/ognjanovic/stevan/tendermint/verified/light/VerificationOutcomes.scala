package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError

object VerificationOutcomes {

  sealed abstract class VerificationOutcome

  case object Success extends VerificationOutcome

  case object InsufficientTrust extends VerificationOutcome

  case class Failure(reason: VerificationError) extends VerificationOutcome

}
