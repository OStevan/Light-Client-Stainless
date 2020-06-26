package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Address, Key}

case class ValidatorInfo(address: Address, pub_key: Key, voting_power: Long)
