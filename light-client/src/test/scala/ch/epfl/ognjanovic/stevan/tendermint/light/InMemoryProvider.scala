package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import io.circe.Decoder

sealed class InMemoryProvider(override val chainId: String, private val map: Map[Height, LightBlock])
    extends LightBlockProvider {

  override def lightBlock(height: Height): LightBlock = map(height)

  override def currentHeight: Height =
    map.keys.max((x: Height, y: Height) => math.signum((x.value - y.value).toLong).toInt)

  override def latestLightBlock: LightBlock = map(currentHeight)
}

object InMemoryProvider {
  val defaultChainId: String = "test-chain-01"

  def defaultChainDecoder(implicit lightBlockDeserializer: Decoder[LightBlock]): Decoder[LightBlockProvider] = cursor =>
    for {
      lightBlocks <- cursor.as[Array[LightBlock]]
    } yield {
      val zipped: Array[(Height, LightBlock)] = lightBlocks.map(block => (block.header.height, block))
      new InMemoryProvider(defaultChainId, zipped.toMap)
    }

  def decoder(implicit lightBlockDeserializer: Decoder[LightBlock]): Decoder[LightBlockProvider] = cursor =>
    for {
      lightBlocks <- cursor.downField("lite_blocks").as[Array[LightBlock]]
      chainId <- cursor.downField("chain_id").as[String]
    } yield {
      val zipped: Array[(Height, LightBlock)] = lightBlocks.map(block => (block.header.height, block))
      new InMemoryProvider(chainId, zipped.toMap)
    }

}
