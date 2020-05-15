package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import io.circe.Decoder

case class Address(address: String)

object Address {
  val deserializer: Deserializer[Address] = new Deserializer[Address] {
    override implicit val decoder: Decoder[Address] = cursor => for {
      value <- cursor.as[String]
    } yield Address(value)
  }
}
