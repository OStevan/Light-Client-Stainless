package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import io.circe.Decoder

case class Validator(address: Address, publicKey: Key, votingPower: Long, priority: Long)

object Validator {
  val deserializer: Deserializer[Validator] = new Deserializer[Validator] {
    override implicit val decoder: Decoder[Validator] = cursor => for {
      address <- cursor.downField("address").as[Address](Address.deserializer.decoder)
      publicKey <- cursor.downField("pub_key").as[Key](Key.deserializer.decoder)
      votingPower <- cursor.downField("voting_power").as[Long]
      proposerPriority <- cursor.downField("proposer_priority").as[Long]
    } yield {
      Validator(address, publicKey, votingPower, proposerPriority)
    }
  }
}

