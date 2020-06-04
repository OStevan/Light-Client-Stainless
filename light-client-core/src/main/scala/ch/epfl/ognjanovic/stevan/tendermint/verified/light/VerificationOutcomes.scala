package ch.epfl.ognjanovic.stevan.tendermint.verified.light

object VerificationOutcomes {

  sealed abstract class VerificationOutcome

  case object Success extends VerificationOutcome

  case object InvalidHeader extends VerificationOutcome

  case object InvalidCommit extends VerificationOutcome

  case object Failure extends VerificationOutcome

  case object InsufficientTrust extends VerificationOutcome

  case object ExpiredTrustedState extends VerificationOutcome

}
