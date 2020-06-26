package ch.epfl.ognjanovic.stevan.tendermint.verified.types

object Validators {

  case class Validator(address: Address, publicKey: Key, votingPower: VotingPower, priority: Long) {
    def toInfoHashable: InfoHashable = InfoHashable(publicKey, votingPower)
  }

  case class InfoHashable(publicKey: Key, votingPower: VotingPower)

}
