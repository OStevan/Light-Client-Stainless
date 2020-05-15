package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import java.nio.ByteBuffer
import java.time.Instant

import io.circe.Decoder


case class Signature(blockIdFlag: Byte, validatorAddress: Address, timestamp: Instant, signature: ByteArray)

object Signature {
  val deserializer: Deserializer[Signature] = new Deserializer[Signature] {
    override implicit val decoder: Decoder[Signature] = cursor => for {
      blockFlagId <- cursor.downField("block_id_flag").as[Byte]
      validatorAddress <- cursor.downField("validator_address").as[Address](Address.deserializer.decoder)
      timestamp <- cursor.downField("timestamp").as[Instant]
      signature <- cursor.downField("signature").as[String]
    } yield {
      Signature(
        blockFlagId,
        validatorAddress,
        timestamp,
        ByteBuffer.wrap(signature.map(_.toByte).toArray).asReadOnlyBuffer())
    }
  }
}