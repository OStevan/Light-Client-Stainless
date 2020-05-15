package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import io.circe.parser.parse
import io.circe.{Decoder, Json}

trait Deserializer[T] {
  implicit val decoder: Decoder[T]

  final def apply(json: Json): T = json.as[T] match {
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
