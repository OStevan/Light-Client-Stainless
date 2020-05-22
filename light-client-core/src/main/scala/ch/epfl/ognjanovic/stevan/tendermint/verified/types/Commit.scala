package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import stainless.annotation.pure
import stainless.collection._
import stainless.lang._
import utils.ListUtils

case class Commit(height: Height, round: Long, blockId: BlockId, signatures: List[Signature]) {
  require(ListUtils.noDuplicate(signatures.map(_.validatorAddress)))

  @pure
  def signers: Set[Address] = signatures.map(_.validatorAddress).toSet
}
