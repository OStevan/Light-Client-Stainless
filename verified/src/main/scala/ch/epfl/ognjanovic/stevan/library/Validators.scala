package ch.epfl.ognjanovic.stevan.library

import stainless.lang._
import stainless.collection._

import ch.epfl.ognjanovic.stevan.library.types.VotingPower._
import ch.epfl.ognjanovic.stevan.library.Nodes.Node

case class Validators(validators: Map[Node, PositiveVotingPower]) {

    def setPower(nodes: Set[Node]): VotingPower = {
        require(nodes subsetOf validators.keys.toSet)
        nodePower(nodes.toList)
    }

    def nodePower(nodes: List[Node]): VotingPower = {
        nodes match {
            case Cons(head, tail) => validators.get(head).power().+(nodePower(tail))
            case Nil() => ZeroVotingPower 
        }
    }
}
