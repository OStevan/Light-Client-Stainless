package ch.epfl.ognjanovic.stevan.blockchain

import ch.epfl.ognjanovic.stevan.types.Nodes._
import ch.epfl.ognjanovic.stevan.types._
import stainless.lang._

object Messages {

  sealed abstract class SystemStep

  case class Initialize(
                         validatorSet: Validators,
                         maxHeight: Height,
                         maxPower: VotingPower,
                         nextValidatorSet: Validators) extends SystemStep {
    require(
      validatorSet.keys.nonEmpty &&
        validatorSet.values.forall(value => value.power == 1) &&
        maxPower.isPositive &&
        nextValidatorSet.keys.nonEmpty &&
        (nextValidatorSet.keys subsetOf validatorSet.keys) &&
        maxHeight.value > BigInt(1))
  }

  /**
   * Models a progression of time in an arbitrary way by specifying by how much the min trusted height sould be
   * increased.When processing this message the system invariants need to be guaranted as they are not maintained
   * here.
   *
   * @param step modeling the progression of time
   */
  case class TimeStep(step: BigInt) extends SystemStep {
    require(step > BigInt(0))
  }
  case class Fault(node: Node) extends SystemStep

  case class AppendBlock(lastCommit: Set[Node], nextValidatorSet: Validators) extends SystemStep {
    require(
      nextValidatorSet.keys.nonEmpty &&
        nextValidatorSet.values.forall(value => value.power == 1) &&
        lastCommit.nonEmpty)
  }
}
