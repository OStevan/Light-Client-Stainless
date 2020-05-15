package ch.epfl.ognjanovic.stevan.tendermint.rpc.types
import io.circe.Decoder

case class ValidatorSet(validators: Array[Validator], proposer: Validator)

object ValidatorSet {
  private implicit val validatorDecoder: Decoder[Validator] = Validator.deserializer.decoder

  val deserializer: Deserializer[ValidatorSet] = new Deserializer[ValidatorSet] {
    override implicit val decoder: Decoder[ValidatorSet] = cursor => for {
      validators <- cursor.downField("validators").as[Array[Validator]]
      proposer <- cursor.downField("proposer").as[Validator]
    } yield {
      ValidatorSet(validators, proposer)
    }
  }
}