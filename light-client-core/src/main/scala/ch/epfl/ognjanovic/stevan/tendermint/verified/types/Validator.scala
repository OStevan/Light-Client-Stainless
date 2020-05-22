package ch.epfl.ognjanovic.stevan.tendermint.verified.types

case class Validator(address: Address, publicKey: Key, votingPower: VotingPower, priority: Long)
