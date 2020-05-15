package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.rpc.types.Deserializer
import io.circe.Decoder

class InMemoryProvider(private val map: Map[Long, LightBlock]) extends Provider {
  override def lightBlock(height: Long): LightBlock = map(height)
}

object InMemoryProvider {
  def deserializer(implicit lightBlockDeserializer: Deserializer[LightBlock]): Deserializer[Provider] =
    new Deserializer[Provider] {
      implicit val lightBlockDecoder: Decoder[LightBlock] = lightBlockDeserializer.decoder
      override implicit val decoder: Decoder[Provider] = cursor => for {
        lightBlocks <- cursor.as[Array[LightBlock]]
      } yield {
        val zipped: Array[(Long, LightBlock)] = lightBlocks.zip(1L to (lightBlocks.length + 1)).map(a => (a._2, a._1))
        new InMemoryProvider(zipped.toMap)
      }
    }
}