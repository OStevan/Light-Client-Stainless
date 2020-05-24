package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import io.circe.Decoder

class InMemoryProvider(private val map: Map[Long, LightBlock]) extends Provider {
  override def lightBlock(height: Long): LightBlock = map(height)
}

object InMemoryProvider {
  def decoder(implicit lightBlockDeserializer: Decoder[LightBlock]): Decoder[Provider] = cursor => for {
    lightBlocks <- cursor.as[Array[LightBlock]]
  } yield {
    val zipped: Array[(Long, LightBlock)] = lightBlocks.zip(1L to (lightBlocks.length + 1)).map(a => (a._2, a._1))
    new InMemoryProvider(zipped.toMap)
  }
}