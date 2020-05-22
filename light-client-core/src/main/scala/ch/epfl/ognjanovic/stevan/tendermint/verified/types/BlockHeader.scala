package ch.epfl.ognjanovic.stevan.tendermint.verified.types

case class BlockHeader(height: Height, lastCommit: Commit, validatorSet: ValidatorSet, nextValidatorSet: ValidatorSet)
