package ch.epfl.ognjanovic.stevan.tendermint.light

import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class LightClientIntegrationTests extends AnyFlatSpec {

  "Parsing json array of LightBlocks" should "construct an in memory provider" in {
    val content = LightClientIntegrationTests.content("/light_blocks/light_blocks_1.json")

    InMemoryProvider.deserializer(LightBlock.deserializer(Address("just_test")))(content)
  }
}

object LightClientIntegrationTests {
  def content(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString finally source.close()
  }
}