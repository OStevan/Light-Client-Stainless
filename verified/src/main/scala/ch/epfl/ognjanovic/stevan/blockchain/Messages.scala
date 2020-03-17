package ch.epfl.ognjanovic.stevan.blockchain

import ch.epfl.ognjanovic.stevan.library.Nodes._
import ch.epfl.ognjanovic.stevan.library.types._
import ch.epfl.ognjanovic.stevan.library._
import stainless.lang._
import stainless.collection._

object Messages {
    sealed abstract class SystemStep
    case class Initialize(nodePowers: NodePowers, maxHeight: Height, maxPower: VotingPower, validators: Validators) extends SystemStep {
        require(
            nodePowers.keys.nonEmpty &&
            nodePowers.values.forall(value => value.power == 1) && 
            maxPower.isPositive &&
            validators.validators.keys.nonEmpty &&
            (validators.validators.keys subsetOf nodePowers.keys))
    }

    /**
      * Models a progression of time in an arbitrary way by specifying by how much the min trusted height sould be increased.
      * When processing this message the system invariants need to be guaranted as they are not maintained here. 
      *
      * @param step modeling the progression of time
      */
    case class TimeStep(step: BigInt) extends SystemStep {
        require(step > BigInt(0))
    }
    case class Fault(node: Node) extends SystemStep

    case class AppendBlock(lastCommit: Set[Node], nodePowers: NodePowers) extends SystemStep {
        require(
            nodePowers.keys.nonEmpty &&
            nodePowers.values.forall(value => value.power == 1) && 
            !lastCommit.isEmpty)
    }
}
