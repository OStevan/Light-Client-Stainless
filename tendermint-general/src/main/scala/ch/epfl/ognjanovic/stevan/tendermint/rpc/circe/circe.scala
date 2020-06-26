package ch.epfl.ognjanovic.stevan.tendermint.rpc.circe

import java.time.Instant

import io.circe.Decoder
import stainless.annotation.ignore

@ignore
object circe {
  type ByteArray = Seq[Byte]

  val hexStringDecoder: Decoder[ByteArray] = cursor =>
    for {
      value <- cursor.as[String]
    } yield {
      value.sliding(2, 2).map(Integer.parseInt(_, 16).toByte).toVector
    }

  implicit val instantDecoder: Decoder[Instant] = cursor =>
    for {
      value <- cursor.as[String]
    } yield {
      Instant.parse(value)
    }

  private[rpc] def toStainlessOption[T](option: Option[T]): stainless.lang.Option[T] = {
    if (option.isDefined)
      stainless.lang.Some(option.get)
    else
      stainless.lang.None()
  }

}
