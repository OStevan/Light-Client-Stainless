package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import io.circe.Decoder

case class Consensus(block: Long, app: Long)

object Consensus {
  def deserializer: Deserializer[Consensus] = new Deserializer[Consensus] {
    override implicit val decoder: Decoder[Consensus] = cursor => for {
      block <- cursor.downField("block").as[Long]
      app <- cursor.downField("app").as[Long]
    } yield {
      Consensus(block, app)
    }
  }
}
