package ch.epfl.ognjanovic.stevan.types

import ch.epfl.ognjanovic.stevan.types.Nodes.Node
import stainless.lang._
import stainless.collection._

case class NodePowers(totalPower: VotingPower, powerAssignments: Map[Node, VotingPower]) {
    require(powerAssignments.values.forall(value => value.isPositive) &&
      totalPower == powerAssignments.values.foldLeft(VotingPower(0))((acc, value) => acc + value) &&
      powerAssignments.keys.nonEmpty)

    def nodePower(node: Node): VotingPower = {
        powerAssignments.getOrElse(node, VotingPower(0))
    } ensuring (res => (contains(node) ==> res.isPositive) || ((!contains(node)) ==> (!res.isPositive)))

    def keys: Set[Node] = powerAssignments.keys.content

    def values: List[VotingPower] = powerAssignments.values

    def contains(node: Node): Boolean = keys.contains(node)

    def apply(node: Node): VotingPower = this.nodePower(node)
}
