package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.CommitSignatures._
import stainless.annotation.pure
import stainless.collection._

case class Commit(height: Height, round: Long, blockId: BlockId, signatures: List[CommitSignature]) {
  require(signatures.nonEmpty)

  @pure
  def forBlock: ListSet[Address] = {
    val list: List[Address] = signatures.flatMap {
      case BlockIDFlagAbsent => List.empty[Address]
      case value: BlockIDFlagCommit => List(value.validator)
      case _: BlockIdFlagNil => List.empty[Address]
    }
    ListSet(ListSetSpec.removeDuplicates(list))
  }

}
