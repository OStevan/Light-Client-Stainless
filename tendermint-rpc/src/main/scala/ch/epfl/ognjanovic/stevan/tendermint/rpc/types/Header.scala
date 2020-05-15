package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import java.time.Instant

import io.circe.Decoder

case class Header(
  version: Consensus,
  chainId: String,
  height: Long,
  time: Instant,
  lastBlockId: BlockId,
  lastCommit: ByteArray,
  data: ByteArray,
  validators: ByteArray,
  nextValidators: ByteArray,
  consensus: ByteArray,
  app: ByteArray,
  lastResults: ByteArray,
  evidence: ByteArray,
  proposer: Address)

object Header {
  def deserializer: Deserializer[Header] = new Deserializer[Header] {
    override implicit val decoder: Decoder[Header] = cursor => for {
      version <- cursor.downField("version").as[Consensus](Consensus.deserializer.decoder)
      chanId <- cursor.downField("chain_id").as[String]
      height <- cursor.downField("height").as[Long]
      time <- cursor.downField("time").as[Instant]
      lastBlockId <- cursor.downField("last_block_id").as[BlockId](BlockId.deserializer.decoder)
      lastCommit <- cursor.downField("last_commit_hash").as[ByteArray](hashDecoder)
      data <- cursor.downField("data_hash").as[ByteArray](hashDecoder)
      validators <- cursor.downField("validators_hash").as[ByteArray](hashDecoder)
      nextValidators <- cursor.downField("next_validators_hash").as[ByteArray](hashDecoder)
      consensus <- cursor.downField("consensus_hash").as[ByteArray](hashDecoder)
      app <- cursor.downField("app_hash").as[ByteArray](hashDecoder)
      lastResults <- cursor.downField("last_results_hash").as[ByteArray](hashDecoder)
      evidence <- cursor.downField("evidence_hash").as[ByteArray](hashDecoder)
      proposer <- cursor.downField("proposer_address").as[Address](Address.deserializer.decoder)
    } yield {
      Header(
        version,
        chanId,
        height,
        time,
        lastBlockId,
        lastCommit,
        data,
        validators,
        nextValidators,
        consensus, app,
        lastResults,
        evidence,
        proposer)
    }
  }
}
