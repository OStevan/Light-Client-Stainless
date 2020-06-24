package ch.epfl.ognjanovic.stevan.tendermint.rpc.circe

import org.scalatest.flatspec.AnyFlatSpec
import stainless.annotation.ignore

import scala.io.Source

@ignore
sealed class TypesDeserialization extends AnyFlatSpec {

  "Key serialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/key.json")

    new CirceDeserializer(CirceDecoders.keyDecoder)(content)
  }

  "Validator deserialization" should "succeed" in {
    val content: String = TypesDeserialization.content("/types/validator.json")

    new CirceDeserializer(CirceDecoders.validatorDecoder)(content)
  }

  "PartSet deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/part.json")

    new CirceDeserializer(CirceDecoders.partSetDecoder)(content)
  }

  "Header deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/header.json")

    new CirceDeserializer(CirceDecoders.headerDecoder)(content)
  }

  "Signature deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/signature.json")

    new CirceDeserializer(CirceDecoders.signatureDecoder)(content)
  }

  "Commit deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/commit.json")

    new CirceDeserializer(CirceDecoders.commitDecoder)(content)
  }

  "ValidatorSet deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/validator_set.json")

    new CirceDeserializer(CirceDecoders.conformanceTestValidatorSetDecoder)(content)
  }

  "SignedHeader deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/signed_header.json")

    new CirceDeserializer(CirceDecoders.signedHeaderDecoder)(content)
  }
}

@ignore
object TypesDeserialization {
  def content(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString finally source.close()
  }
}
