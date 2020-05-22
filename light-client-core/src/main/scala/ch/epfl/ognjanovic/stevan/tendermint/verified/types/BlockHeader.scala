package ch.epfl.ognjanovic.stevan.tendermint.verified.types

case class BlockHeader(header: Header, lastCommit: Commit, validatorSet: ValidatorSet, nextValidatorSet: ValidatorSet)
