package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import java.nio.ByteBuffer
import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.BlockIdFlags.{BlockIDFlagCommit, BlockIdFlag}
import stainless.annotation.{extern, pure}
import stainless.lang._

case class CommitSignature(
  blockIdFlag: BlockIdFlag,
  validatorAddress: Option[Address],
  @extern @pure timestamp: Instant,
  @extern @pure signature: Option[ByteBuffer]) {
  require((blockIdFlag == BlockIDFlagCommit) ==> (validatorAddress.isDefined && signature.isDefined))
}
