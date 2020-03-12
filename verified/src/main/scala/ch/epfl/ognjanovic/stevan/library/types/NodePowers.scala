package ch.epfl.ognjanovic.stevan.library.types

import ch.epfl.ognjanovic.stevan.library.Nodes._
import stainless.annotation._
import stainless.lang._
import stainless.collection._

case class NodePowers(totalPower: VotingPower, powerAssigments: Map[Node, VotingPower]) {
    require(powerAssigments.values.forall(value => value.isPositive) && totalPower == powerAssigments.values.foldLeft(VotingPower(0))((acc, value) => acc + value))

    @inlineOnce
    def nodes = powerAssigments.keys.content
    
    def nodePower(node: Node): VotingPower = {
        powerAssigments.getOrElse(node, VotingPower(0))
    } ensuring(res => (contains(node) ==> res.isPositive) || ((!contains(node)) ==> (!res.isPositive)))

    def keys: List[Node] = powerAssigments.keys

    def values: List[VotingPower] = powerAssigments.values

    def contains(node: Node) = nodes.contains(node)

    def apply(node: Node) = this.nodePower(node)
}
