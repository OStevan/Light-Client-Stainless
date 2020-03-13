package ch.epfl.ognjanovic.stevan.library

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import ch.epfl.ognjanovic.stevan.library.Nodes.Node
import ch.epfl.ognjanovic.stevan.library.types.VotingPower
import ch.epfl.ognjanovic.stevan.library.types.NodePowers

case class Validators(validators: NodePowers) {
    def obtainedByzantineQuorum(nodes: Set[Node]): Boolean = {
        require(nodes subsetOf validators.keys)
        nodesPower(nodes) * VotingPower(3) > validators.totalPower * VotingPower(2)
    }

    def nodesPower(nodes: Set[Node]): VotingPower = {
        require(nodes subsetOf validators.keys)
        nodes.toList.foldLeft(VotingPower(0))((acc, value) => acc + validators(value))
    }
}