package ch.epfl.ognjanovic.stevan.library

import stainless.lang._
import stainless.collection._

import ch.epfl.ognjanovic.stevan.library.types.VotingPowers._
import ch.epfl.ognjanovic.stevan.library.Nodes.Node

case class Validators(validators: Map[Node, PositiveVotingPower]) {

    def setPower(nodes: Set[Node]): VotingPower = {
        require(nodes subsetOf validators.keys.toSet)
        nodePower(nodes.toList)
    }

    // def obtainedByzantineQuorum(nodes: Set[Node]): Boolean = {
    //     // require(nodes subsetOf validators.keys.toSet)
    //     // val totalPower = ZeroVotingPower
    //     // validators.values
    //         // .foldLeft(ZeroVotingPower.asInstanceOf[VotingPower])((first, second) => first + second)
    //     // val votedPower = setPower(nodes)
    //     // (votedPower.*(3)) > (totalPower.*(2))
    //     true
    // }

    def nodePower(nodes: List[Node]): VotingPower = {
        nodes match {
            case Cons(head, tail) => validators.get(head).power().+(nodePower(tail))
            case Nil() => ZeroVotingPower 
        }
    }
}
