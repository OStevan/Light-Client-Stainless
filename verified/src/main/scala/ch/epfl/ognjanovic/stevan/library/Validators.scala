package ch.epfl.ognjanovic.stevan.library

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import ch.epfl.ognjanovic.stevan.library.types.VotingPowers._
import ch.epfl.ognjanovic.stevan.library.Nodes.Node
import ch.epfl.ognjanovic.stevan.library.types.VotingPowers

case class Validators(validators: Map[Node, PositiveVotingPower]) {
    val totalPower: VotingPower = validators.values.
      foldLeft(VotingPowers.zero())((acc, value) => acc + value)

    def obtainedByzantineQuorum(nodes: List[Node]): Boolean = {
        require((nodes.content.toList.size == nodes.size) && (nodes.content subsetOf validators.keys.content))
        nodesPower(nodes) * VotingPowers.positive(3) > totalPower * VotingPowers.positive(2)
    }

    @induct
    def nodesPower(nodes: List[Node]): VotingPower = {
        require(nodes.content subsetOf validators.keys.content)
        nodes match {
            case Cons(head, tail) => 
                assert(validators.contains(head))
                validators.get(head).power + nodesPower(tail)
            case Nil() => ZeroVotingPower 
        }
    }
}
