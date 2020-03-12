package ch.epfl.ognjanovic.stevan.library

import stainless.lang._
import stainless.collection._
import stainless.annotation._
import ch.epfl.ognjanovic.stevan.library.Nodes.Node
import ch.epfl.ognjanovic.stevan.library.types.VotingPower
import ch.epfl.ognjanovic.stevan.library.types.NodePowers

case class Validators(nodePowers: NodePowers) {

    def obtainedByzantineQuorum(nodes: Set[Node]): Boolean = {
        require(nodes subsetOf nodePowers.keys.content)
        nodesPower(nodes) * VotingPower(3) > nodePowers.totalPower * VotingPower(2)
    }

    def nodesPower(nodes: Set[Node]): VotingPower = {
        require(nodes subsetOf nodePowers.keys.content)
        nodes.toList.map(key => nodePowers(key)).foldLeft(VotingPower(0))((acc, value) => acc + value)
    } ensuring(res => nodePowers.totalPower >= res)
}