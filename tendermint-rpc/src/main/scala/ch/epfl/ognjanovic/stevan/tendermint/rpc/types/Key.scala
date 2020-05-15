package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import java.nio.ByteBuffer

import io.circe.Decoder

case class Key(tpe: String, value: ByteArray)

object Key {
  val deserializer: Deserializer[Key] = new Deserializer[Key] {
    override implicit val decoder: Decoder[Key] = cursor => for {
      tpe <- cursor.downField("type").as[String]
      stringValue <- cursor.downField("value").as[String]
    } yield {
      Key(tpe, ByteBuffer.wrap(stringValue.map(_.toByte).toArray).asReadOnlyBuffer())
    }
  }
}