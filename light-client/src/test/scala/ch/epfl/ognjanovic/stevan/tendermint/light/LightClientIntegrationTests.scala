package ch.epfl.ognjanovic.stevan.tendermint.light

import java.nio.ByteBuffer

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDeserializer
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Key, LightBlock, PeerId}
import io.circe.Decoder
import org.scalatest.flatspec.AnyFlatSpec

import scala.io.Source

class LightClientIntegrationTests extends AnyFlatSpec {

  "Parsing json array of LightBlocks" should "construct an in memory provider" in {
    val content = LightClientIntegrationTests.content("/light_blocks/light_blocks_1.json")

    new CirceDeserializer(InMemoryProvider.decoder(LightClientIntegrationTests.lightBlockDecoder))(content)
  }
}

object LightClientIntegrationTests {
  implicit val lightBlockDecoder: Decoder[LightBlock] =
    LightBlockDecoder.decoder(LightClientIntegrationTests.defaultProvider)

  val defaultProvider: PeerId =
    PeerId(
      Key(
        "tendermint/PubKeyEd25519",
        ByteBuffer.wrap("OAaNq3DX/15fGJP2MI6bujt1GRpvjwrqIevChirJsbc=".getBytes).asReadOnlyBuffer()))

  def content(path: String): String = {
    val source = Source.fromURL(getClass.getResource(path))
    try source.mkString finally source.close()
  }
}