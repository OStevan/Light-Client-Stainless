package ch.epfl.ognjanovic.stevan.tendermint.rpc.circe

import java.nio.ByteBuffer
import java.time.Instant

import io.circe.Decoder

@ignore
object circe {
  type ByteArray = ByteBuffer

  val hashDecoder: Decoder[ByteArray] = cursor => for {
    value <- cursor.as[String]
  } yield {
    {
      if (value.isEmpty)
        ByteBuffer.allocate(0)
      else
        ByteBuffer.wrap(BigInt(value, 16).toByteArray)
    }.asReadOnlyBuffer()
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
