package ch.epfl.ognjanovic.stevan.tendermint.rpc.circe

import ch.epfl.ognjanovic.stevan.tendermint.rpc.Deserializer
import io.circe.parser.parse
import io.circe.{Decoder, Json}
import stainless.annotation.ignore

@ignore
class CirceDeserializer[T](private val decoder: Decoder[T]) extends Deserializer[T] {

  final def apply(json: Json): T = json.as[T](decoder) match {
    case Left(error) => throw new IllegalArgumentException(error)
    case Right(value) => value
  }

  final def apply(jsonString: String): T = (for {
    result <- parse(jsonString)
  } yield {
    apply(result)
  }) match {
    case Left(error) => throw new IllegalArgumentException(error)
    case Right(value) => value
  }
}
