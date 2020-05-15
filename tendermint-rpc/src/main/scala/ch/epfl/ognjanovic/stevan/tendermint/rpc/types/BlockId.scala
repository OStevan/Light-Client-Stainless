package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import io.circe.Decoder

case class BlockId(bytes: ByteArray, parts: PartSetHeader)

object BlockId {
  val deserializer: Deserializer[BlockId] = new Deserializer[BlockId] {
    override implicit val decoder: Decoder[BlockId] = cursor => for {
      hash <- cursor.downField("hash").as[ByteArray](hashDecoder)
      parts <- cursor.downField("parts").as[PartSetHeader](PartSetHeader.deserializer.decoder)
    } yield {
      BlockId(hash, parts)
    }
  }
}
