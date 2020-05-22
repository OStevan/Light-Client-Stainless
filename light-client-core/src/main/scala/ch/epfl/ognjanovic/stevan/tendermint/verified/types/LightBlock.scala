package ch.epfl.ognjanovic.stevan.tendermint.verified.types

case class LightBlock(header: Header, commit: Commit, validatorSet: ValidatorSet, nextValidatorSet: ValidatorSet)