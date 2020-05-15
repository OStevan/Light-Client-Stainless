package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import io.circe.Decoder

case class SignedHeader(header: Header, commit: Commit)

object SignedHeader {
  val deserializer: Deserializer[SignedHeader] = new Deserializer[SignedHeader] {
    override implicit val decoder: Decoder[SignedHeader] = cursor => for {
      header <- cursor.downField("header").as[Header](Header.deserializer.decoder)
      commit <- cursor.downField("commit").as[Commit](Commit.deserializer.decoder)
    } yield {
      SignedHeader(header, commit)
    }
  }
}
