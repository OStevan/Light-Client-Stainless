package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Address
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class LightClientIntegrationTests extends AnyFlatSpec {

  "Parsing json array of LightBlocks" should "construct an in memory provider" in {
    val content = LightClientIntegrationTests.content("/light_blocks/light_blocks_1.json")

    new CirceDeserializer(InMemoryProvider.decoder(LightBlock.decoder(Address("just_test"))))(content)
  }
}

object LightClientIntegrationTests {
  def content(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString finally source.close()
  }
}