package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.BlockIdFlags.BlockIDFlagCommit
import stainless.annotation.pure
import stainless.collection._
import stainless.lang._
import utils.ListUtils

case class Commit(height: Height, round: Long, blockId: BlockId, signatures: List[CommitSignature]) {
  require(ListUtils.noDuplicate(signatures.map(_.validatorAddress)))

  @pure
  def committingSigners: Set[Address] =
    signatures
      .filter(value => value.blockIdFlag == BlockIDFlagCommit)
      .map(value => {
        require(value.blockIdFlag == BlockIDFlagCommit)
        value.validatorAddress.get
      })
      .toSet
}
