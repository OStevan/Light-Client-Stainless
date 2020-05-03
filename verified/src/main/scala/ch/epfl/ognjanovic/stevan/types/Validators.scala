package ch.epfl.ognjanovic.stevan.types

import ch.epfl.ognjanovic.stevan.types.Nodes.Node
import stainless.annotation.{extern, induct, opaque, pure}
import stainless.collection._
import stainless.lang._
import stainless.proof._
import utils.ListSetUtils._
import utils.{ListMap, ListSetUtils, ListUtils}

case class Validators(totalPower: VotingPower, powerAssignments: ListMap[Node, VotingPower]) {
  require(powerAssignments.forall(value => value._2.isPositive) &&
    totalPower == Validators.sumVotingPower(powerAssignments.toList) &&
    !powerAssignments.isEmpty)

  @pure @extern
  def keys: Set[Node] = powerAssignments.toList.map(_._1).content

  def values: List[VotingPower] = powerAssignments.toList.map(_._2)

  def contains(node: Node): Boolean = keys.contains(node)

  def nodesPower(nodes: List[Node]): VotingPower = {
    require(nodes.forall(powerAssignments.toList.map(_._1).contains))
    Validators.sumVotingPower(powerAssignments.toList.filter(pair => nodes.contains(pair._1)))
  }

  @pure
  def obtainedByzantineQuorum(nodes: Set[Node]): Boolean = {
    require(nodes subsetOf keys)
    val nodeList = Validators.nodeListContainment(nodes, this)
    nodesPower(nodeList) * VotingPower(3) > totalPower * VotingPower(2)
  }

  @pure
  def isCorrect(faultyNodes: Set[Node]): Boolean = {
    val keySet = powerAssignments.toList.map(_._1)
    val faultyNodesSet = faultyNodes.toList
    val difference = removingFromSet(keySet, faultyNodesSet)
    val intersection = keySet & faultyNodesSet
    setIntersectionLemma(keySet, faultyNodesSet)
    nodesPower(difference) > nodesPower(intersection) * VotingPower(2)
  }

  def checkSupport(nodes: Set[Node]): Boolean = {
    require(nodes subsetOf keys)
    val nodeList = Validators.nodeListContainment(nodes, this)
    ListSetUtils.selfContainment(powerAssignments.toList.map(_._1))
    VotingPower(3) * nodesPower(nodeList) > totalPower
  }
}

object Validators {

  def sumVotingPower(votingPowers: List[(Node, VotingPower)]): VotingPower = votingPowers match {
    case Nil() => VotingPower(0)
    case Cons(head, tail) => head._2 + sumVotingPower(tail)
  }

  @extern
  private def nodeListContainment(set: Set[Node], validators: Validators): List[Node]= {
    require(set subsetOf validators.keys)
    set.toList
  }.ensuring(res => res.forall(validators.powerAssignments.toList.map(_._1).contains))

  @extern
  def moreFaultyDoesNotHelp(current: Set[Node], next: Set[Node]): Unit = {
    require(current subsetOf next)
  }.ensuring(_ => forall((validator: Validators) => !validator.isCorrect(current) ==> !validator.isCorrect(next)))
}