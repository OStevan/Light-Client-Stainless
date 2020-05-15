package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

sealed class TypesDeserialization extends AnyFlatSpec {

  "Key serialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/key.json")

    Key.deserializer(content)
  }

  "Validator deserialization" should "succeed" in {
    val content: String = TypesDeserialization.content("/types/validator.json")

    Validator.deserializer(content)
  }

  "PartSet deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/part.json")

    PartSetHeader.deserializer(content)
  }

  "Header deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/header.json")

    Header.deserializer(content)
  }

  "Signature deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/signature.json")

    Signature.deserializer(content)
  }

  "Commit deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/commit.json")

    Commit.deserializer(content)
  }

  "ValidatorSet deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/validator_set.json")

    ValidatorSet.deserializer(content)
  }

  "SignedHeader deserialization" should "succeed" in {
    val content = TypesDeserialization.content("/types/signed_header.json")

    SignedHeader.deserializer(content)
  }
}

object TypesDeserialization {
  def content(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString finally source.close()
  }
}
