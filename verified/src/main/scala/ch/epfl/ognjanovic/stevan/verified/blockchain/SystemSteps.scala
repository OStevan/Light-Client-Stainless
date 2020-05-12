package ch.epfl.ognjanovic.stevan.verified.blockchain

import ch.epfl.ognjanovic.stevan.verified.types.Nodes._
import ch.epfl.ognjanovic.stevan.verified.types.Validators
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
  case class Fault(node: Node) extends SystemStep

  /**
   * Models a new block event of the blockchain.
   *
   * @param lastCommit       the set of nodes which agreed to commit the last block
   * @param nextValidatorSet an agreed set of validators for the next block
   */
  case class AppendBlock(lastCommit: Set[Node], nextValidatorSet: Validators) extends SystemStep {
    require(
      nextValidatorSet.keys.nonEmpty &&
        nextValidatorSet.values.forall(value => value.power == 1) &&
        lastCommit.nonEmpty)
  }

}
