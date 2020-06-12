package ch.epfl.ognjanovic.stevan.tendermint.verified.light

object VerificationErrors {

  sealed abstract class VerificationError

  case object NonMonotonicBftTime extends VerificationError

  case object InvalidHeader extends VerificationError

  case object InvalidCommit extends VerificationError

  case object InsufficientCommitPower extends VerificationError

  case object InvalidCommitValue extends VerificationError

  case object InvalidNextValidatorSet extends VerificationError

  case object InvalidValidatorSetHash extends VerificationError

  case object InvalidNextValidatorSetHash extends VerificationError

  case object InvalidCommitVoteSignature extends VerificationError

  case object ExpiredTrustedState extends VerificationError

}
