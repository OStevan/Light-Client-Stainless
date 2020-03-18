package ch.epfl.ognjanovic.stevan.types

import ch.epfl.ognjanovic.stevan.types.Nodes.Node

case class NodePowers(totalPower: VotingPower, powerAssigments: Map[Node, VotingPower]) {
    require(powerAssigments.values.forall(value => value.isPositive) &&
      totalPower == powerAssigments.values.foldLeft(VotingPower(0))((acc, value) => acc + value) &&
      powerAssigments.keys.nonEmpty)

    def nodePower(node: Node): VotingPower = {
        powerAssigments.getOrElse(node, VotingPower(0))
    } ensuring (res => (contains(node) ==> res.isPositive) || ((!contains(node)) ==> (!res.isPositive)))

    def keys: Set[Node] = powerAssigments.keys.content

    def values: List[VotingPower] = powerAssigments.values

    def contains(node: Node): Boolean = keys.contains(node)

    def apply(node: Node): VotingPower = this.nodePower(node)
}
