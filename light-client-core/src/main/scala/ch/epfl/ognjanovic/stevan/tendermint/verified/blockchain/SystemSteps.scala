package ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Address, Commit, ValidatorSet}

object SystemSteps {

  /**
   * Models an event which can happen in a blockchain system
   */
  sealed abstract class SystemStep

  /**
   * Models time progression by a specific time delta defined with seconds and nanos.
   * @param seconds passed
   * @param nanos passed
   */
  case class TimeStep(seconds: BigInt, nanos: BigInt) extends SystemStep {
    require(seconds > 0 && nanos > 0)
  }

  /**
   * Models an arbitrary fault event of a single node. Assumption is that failed nodes will not recover.
   *
   * @param node which is faulty
   */
  case class Fault(node: Address) extends SystemStep

  /**
   * Models a new block event of the blockchain.
   *
   * @param lastCommit       commit for the last block
   * @param nextValidatorSet an agreed set of validators for the next block
   */
  case class AppendBlock(lastCommit: Commit, nextValidatorSet: ValidatorSet) extends SystemStep {
    require(nextValidatorSet.values.forall(_.votingPower.value == 1) && lastCommit.forBlock.nonEmpty)
  }

}
