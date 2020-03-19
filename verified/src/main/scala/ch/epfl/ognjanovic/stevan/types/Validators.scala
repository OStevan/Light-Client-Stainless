package ch.epfl.ognjanovic.stevan.types

import ch.epfl.ognjanovic.stevan.types.Nodes.Node
import stainless.lang._
import stainless.collection._

case class Validators(totalPower: VotingPower, powerAssignments: Map[Node, VotingPower]) {
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

  def nodesPower(nodes: Set[Node]): VotingPower = {
    require(nodes subsetOf keys)
    nodes.toList.foldLeft(VotingPower(0))((acc, value) => acc + apply(value))
  }

  def obtainedByzantineQuorum(nodes: Set[Node]): Boolean = {
    require(nodes subsetOf keys)
    nodesPower(nodes) * VotingPower(3) > totalPower * VotingPower(2)
  }

  def isCorrect(faultyNodes: Set[Node]): Boolean = {
    nodesPower(keys -- faultyNodes) > nodesPower(keys & faultyNodes) * VotingPower(2)
  }
}