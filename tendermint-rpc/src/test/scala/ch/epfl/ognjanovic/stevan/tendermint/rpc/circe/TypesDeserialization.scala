package ch.epfl.ognjanovic.stevan.tendermint.rpc.circe

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

sealed class TypesDeserialization extends AnyFlatSpec {

  "Key serialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/key.json")

    new Deserializer(CirceDecoders.keyDecoder)(content)
  }

  "Validator deserialization" should "succeed" in {
    val content: String = TypesDeserialization.content("/types/validator.json")

    new Deserializer(CirceDecoders.validatorDecoder)(content)
  }

  "PartSet deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/part.json")

    new Deserializer(CirceDecoders.partSetDecoder)(content)
  }

  "Header deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/header.json")

    new Deserializer(CirceDecoders.headerDecoder)(content)
  }

  "Signature deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/signature.json")

    new Deserializer(CirceDecoders.signatureDecoder)(content)
  }

  "Commit deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/commit.json")

    new Deserializer(CirceDecoders.commitDecoder)(content)
  }

  "ValidatorSet deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/validator_set.json")

    new Deserializer(CirceDecoders.validatorSetDecoder)(content)
  }

  "SignedHeader deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/signed_header.json")

    new Deserializer(CirceDecoders.signedHeaderDecoder)(content)
  }
}

object TypesDeserialization {
  def content(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString finally source.close()
  }
}
