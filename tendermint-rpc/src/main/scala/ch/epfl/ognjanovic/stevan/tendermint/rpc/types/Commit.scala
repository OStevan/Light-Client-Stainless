package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import io.circe.Decoder

case class Commit(height: Long, round: Long, blockId: BlockId, signatures: Array[Signature])

object Commit {
  private implicit val signatureDecoder: Decoder[Signature] = Signature.deserializer.decoder

  val deserializer: Deserializer[Commit] = new Deserializer[Commit] {
    override implicit val decoder: Decoder[Commit] = cursor => for {
      height <- cursor.downField("height").as[Long]
      round <- cursor.downField("round").as[Long]
      blockId <- cursor.downField("block_id").as[BlockId](BlockId.deserializer.decoder)
      signatures <- cursor.downField("signatures").as[List[Signature]]
    } yield {
      Commit(height, round, blockId, signatures.toArray)
    }
  }
}
