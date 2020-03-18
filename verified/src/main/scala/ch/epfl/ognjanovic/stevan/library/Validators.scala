package ch.epfl.ognjanovic.stevan.library

import ch.epfl.ognjanovic.stevan.library.Nodes.Node

case class Validators(validators: NodePowers) {

    def nodesPower(nodes: Set[Node]): VotingPower = {
        require(nodes subsetOf validators.keys)
        nodes.toList.foldLeft(VotingPower(0))((acc, value) => acc + validators(value))
    }

    def obtainedByzantineQuorum(nodes: Set[Node]): Boolean = {
        require(nodes subsetOf validators.keys)
        nodesPower(nodes) * VotingPower(3) > validators.totalPower * VotingPower(2)
    }

    def isCorrect(faultyNodes: Set[Node]): Boolean = {
        nodesPower(validators.keys -- faultyNodes) > nodesPower(validators.keys & faultyNodes) * VotingPower(2)
    }
}