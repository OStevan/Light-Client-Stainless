package ch.epfl.ognjanovic.stevan.tendermint.verified.light

object VerificationErrors {

  sealed abstract class VerificationError

  case object InvalidHeader extends VerificationError

  case object InvalidCommit extends VerificationError

  case object InvalidNextValidatorSet extends VerificationError

  case object ExpiredTrustedState extends VerificationError
}
