package ch.epfl.ognjanovic.stevan.tendermint.rpc.circe

import java.nio.ByteBuffer
import java.time.Instant

import io.circe.Decoder
import stainless.annotation.ignore

@ignore
object circe {
  type ByteArray = ByteBuffer

  val hexStringDecoder: Decoder[ByteArray] = cursor => for {
    value <- cursor.as[String]
  } yield {
    ByteBuffer.wrap(value.sliding(2, 2).map(Integer.parseInt(_, 16).toByte).toArray)
  }

  implicit val instantDecoder: Decoder[Instant] = cursor => for {
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
