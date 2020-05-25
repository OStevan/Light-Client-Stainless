package ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Address, Commit, ValidatorSet}
import stainless.lang._

object SystemSteps {

  /**
   * Models an event which can happen in a blockchain system
   */
  sealed abstract class SystemStep

  /**
   * Models a progression of time in an arbitrary way by specifying by how much the min trusted height should be
   * increased. This is in line with how we think of BFT time in terms of the Tendermint blockchain. When processing
   * this message the system invariants need to be guaranteed as they are not maintained here.
   *
   * @param step modeling the progression of time
   */
  case class TimeStep(step: BigInt) extends SystemStep {
    require(step > BigInt(0))
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
    require(nextValidatorSet.values.forall(_.votingPower.value == 1) && lastCommit.committingSigners.nonEmpty)
  }

}
