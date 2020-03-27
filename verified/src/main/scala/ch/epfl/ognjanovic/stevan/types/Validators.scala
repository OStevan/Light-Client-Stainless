package ch.epfl.ognjanovic.stevan.types

import ch.epfl.ognjanovic.stevan.types.Nodes.Node
import ch.epfl.ognjanovic.stevan.utils.ListMap
import stainless.lang._
import stainless.collection._

case class Validators(totalPower: VotingPower, powerAssignments: ListMap[Node, VotingPower]) {
  require(powerAssignments.forall(value => value._2.isPositive) &&
    totalPower == powerAssignments.toList.foldLeft(VotingPower(0))((acc, value) => acc + value._2) &&
    !powerAssignments.isEmpty)

  def nodePower(node: Node): VotingPower = {
    if (powerAssignments.contains(node))
      powerAssignments(node)
    else
      VotingPower(0)
  } ensuring (res => (contains(node) ==> res.isPositive) || ((!contains(node)) ==> (!res.isPositive)))

  def keys: Set[Node] = powerAssignments.toList.map(_._1).content

  def values: List[VotingPower] = powerAssignments.toList.map(_._2)

  def contains(node: Node): Boolean = keys.contains(node)

  def apply(node: Node): VotingPower = {
    nodePower(node)
  }

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