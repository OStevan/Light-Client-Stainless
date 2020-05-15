package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import io.circe.Decoder

case class PartSetHeader(total: Int, hash: ByteArray)

object PartSetHeader {
  val deserializer: Deserializer[PartSetHeader] = new Deserializer[PartSetHeader] {
    override implicit val decoder: Decoder[PartSetHeader] = cursor => for {
      total <- cursor.downField("total").as[Int]
      hash <- cursor.downField("hash").as[ByteArray](hashDecoder)
    } yield {
      PartSetHeader(total, hash)
    }
  }
}
