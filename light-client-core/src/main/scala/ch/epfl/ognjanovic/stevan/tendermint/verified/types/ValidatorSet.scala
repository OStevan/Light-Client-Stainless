package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import stainless.annotation._
import stainless.collection._
import stainless.lang._
import stainless.proof._
import utils.ListSetUtils._
import utils._

case class ValidatorSet(totalPower: VotingPower, powerAssignments: ListMap[Address, Validator], proposer: Validator) {
  require(
    powerAssignments.forall(value => value._2.votingPower.isPositive) &&
      totalPower == ValidatorSet.sumVotingPower(powerAssignments.toList) &&
      !powerAssignments.isEmpty)

  @pure @extern
  def keys: Set[Address] = {
    powerAssignments.toList.map(_._1).content
  }.ensuring(res => res.nonEmpty)

  def values: List[Validator] = powerAssignments.toList.map(_._2)

  def contains(node: Address): Boolean = keys.contains(node)

  def nodesPower(nodes: List[Address]): VotingPower = {
    require(nodes.forall(powerAssignments.toList.map(_._1).contains))
    ValidatorSet.sumVotingPower(powerAssignments.toList.filter(pair => nodes.contains(pair._1)))
  }

  @pure
  def obtainedByzantineQuorum(nodes: Set[Address]): Boolean = {
    require(nodes subsetOf keys)
    val nodeList = ValidatorSet.nodeListContainment(nodes, this)
    nodesPower(nodeList) * VotingPower(3) > totalPower * VotingPower(2)
  }

  @pure
  def isCorrect(faultyNodes: Set[Address]): Boolean = {
    val keySet = powerAssignments.toList.map(_._1)
    val faultyNodesSet = faultyNodes.toList
    val difference = removingFromSet(keySet, faultyNodesSet)
    val intersection = keySet & faultyNodesSet
    setIntersectionLemma(keySet, faultyNodesSet)
    nodesPower(difference) > nodesPower(intersection) * VotingPower(2)
  }

  def checkSupport(nodes: Set[Address]): Boolean = {
    require(nodes subsetOf keys)
    val nodeList = ValidatorSet.nodeListContainment(nodes, this)
    ListSetUtils.selfContainment(powerAssignments.toList.map(_._1))
    VotingPower(3) * nodesPower(nodeList) > totalPower
  }
}

object ValidatorSet {

  @pure
  def sumVotingPower(votingPowers: List[(Address, Validator)]): VotingPower = votingPowers match {
    case Nil() => VotingPower(0)
    case Cons(head, tail) => head._2.votingPower + sumVotingPower(tail)
  }

  @extern
  private def nodeListContainment(set: Set[Address], validators: ValidatorSet): List[Address] = {
    require(set subsetOf validators.keys)
    set.toList
  }.ensuring(res => res.forall(validators.powerAssignments.toList.map(_._1).contains))

  @opaque
  def moreFaultyDoesNotHelp(current: Set[Address], next: Set[Address]): Unit = {
    require(current subsetOf next)
  }.ensuring(_ =>
    forall((validator: ValidatorSet) => {
      faultyExpansion(next, current, validator)
      !validator.isCorrect(current) ==> !validator.isCorrect(next)
    }))

  // TODO if isCorrect is inlined this lemma can be proved but calls to isCorrect at other locations fail
  @extern
  def faultyExpansion(next: Set[Address], current: Set[Address], validators: ValidatorSet): Unit = {
    require(current subsetOf next)
    val keys = validators.powerAssignments.toList.map(_._1)
    val nextList = listSetSubsetEquivalence(next)
    val currentList = listSetSubsetEquivalence(current)
    val currentDiff = removingFromSet(keys, currentList)
    val nextDiff = removingFromSet(keys, nextList)

    val difference_proof = {
      expandPredicate(currentList, currentList.contains, nextList.contains)
      subsetRemovingLemma(keys, currentList, nextList)
      subsetPowerLemma(nextDiff, currentDiff, validators)
      check(validators.nodesPower(currentDiff) >= validators.nodesPower(nextDiff))
    }

    val nextIntersection = keys & nextList
    val currentIntersection = keys & currentList
    val intersection_proof = {
      setIntersectionLemma(keys, nextList)
      setIntersectionLemma(keys, currentList)
      intersectionContainmentLemma(keys, currentList)
      intersectionContainmentLemma(keys, nextList)
      selfContainment(nextIntersection)
      selfContainment(currentIntersection)

      setIntersectionContainmentLemma(keys, currentList, nextList)
      subsetPowerLemma(currentIntersection, nextIntersection, validators)
      check(validators.nodesPower(currentIntersection) <= validators.nodesPower(nextIntersection))
    }

    correctLemma(
      validators.nodesPower(currentDiff),
      validators.nodesPower(currentIntersection),
      validators.nodesPower(nextDiff),
      validators.nodesPower(nextIntersection))

    assert(!(validators.nodesPower(currentDiff) > validators.nodesPower(currentIntersection) * VotingPower(2)) ==>
      !(validators.nodesPower(nextDiff) > validators.nodesPower(nextIntersection) * VotingPower(2)))
    assert(validators.isCorrect(current) ==
      validators.nodesPower(currentDiff) > validators.nodesPower(currentIntersection) * VotingPower(2))
    assert(validators.isCorrect(next) ==
      validators.nodesPower(nextDiff) > validators.nodesPower(nextIntersection) * VotingPower(2))
  }.ensuring(_ => !validators.isCorrect(current) ==> !validators.isCorrect(next))

  @opaque
  def correctLemma(
    firstCorrect: VotingPower,
    firstFaulty: VotingPower,
    secondCorrect: VotingPower,
    secondFaulty: VotingPower): Unit = {
    require(firstCorrect >= secondCorrect && firstFaulty <= secondFaulty)
  }.ensuring(_ => !(firstCorrect > firstFaulty * VotingPower(2)) ==> !(secondCorrect > secondFaulty * VotingPower(2)))

  @opaque
  def subsetPowerLemma(first: List[Address], second: List[Address], validators: ValidatorSet): Unit = {
    require(
      first.forall(second.contains) &&
        second.forall(validators.powerAssignments.toList.map(_._1).contains) &&
        ListUtils.noDuplicate(first) &&
        ListUtils.noDuplicate(second)
    )
    transitivityLemma(first, second, validators.powerAssignments.toList.map(_._1))
    val firstFiltered = validators.powerAssignments.toList.filter(node => first.contains(node._1))
    uniquenessTransitivity(validators.powerAssignments.toList)
    filteringPreservesPredicate(first, validators.powerAssignments.toList)

    val secondFiltered = validators.powerAssignments.toList.filter(node => second.contains(node._1))
    filteringPreservesPredicate(second, validators.powerAssignments.toList)

    subsetFilteringCreatesSubsets(first, second, validators.powerAssignments.toList)

    subsetSumLessEq(firstFiltered, secondFiltered)
  }.ensuring(_ => validators.nodesPower(first) <= validators.nodesPower(second))

  @opaque
  def subsetSumLessEq(@induct first: List[(Address, Validator)], second: List[(Address, Validator)]): Unit = {
    require(first.forall(second.contains) && ListUtils.noDuplicate(first) && ListUtils.noDuplicate(second))
    val difference = removingFromSet(second, first)
    sumWithDifferenceIsEqual(first, second)
    appendSameAsAddition(first, difference)
    appendIncreases(first, difference)
  }.ensuring(_ => sumVotingPower(first) <= sumVotingPower(second))

  @opaque
  def sumWithDifferenceIsEqual(first: List[(Address, Validator)], second: List[(Address, Validator)]): Unit = {
    require(first.forall(second.contains) && ListUtils.noDuplicate(first) && ListUtils.noDuplicate(second))
    second match {
      case Nil() => ()

      case Cons(h, t) if first.contains(h) =>
        val removed = first - h
        removeOne(h, first)
        interestingEquality(h, first, t)
        listSetRemoveHeadSameIsSubtraction(second)
        removingContainment(h, first, second)
        sumWithDifferenceIsEqual(removed, t)

      case Cons(_, t) =>
        doesNotHaveHeadContainedInTail(first, second)
        sumWithDifferenceIsEqual(first, t)
    }
  }.ensuring(_ => sumVotingPower(first) + sumVotingPower(second -- first) == sumVotingPower(second))

  @opaque
  def removeOne(elem: (Address, Validator), list: List[(Address, Validator)]): Unit = {
    require(ListUtils.noDuplicate(list) && list.contains(elem) && list.nonEmpty)
    list match {
      case Cons(_, Nil()) => ()
      case Cons(_, tail) if !tail.contains(elem) => removingNonContained(tail, elem)
      case Cons(_, tail) => removeOne(elem, tail)
    }
  }.ensuring(_ =>
    sumVotingPower(list) == elem._2.votingPower + sumVotingPower(list - elem) && ListUtils.noDuplicate(list - elem))

  @opaque
  def appendIncreases(first: List[(Address, Validator)], second: List[(Address, Validator)]): Unit = {
    additionLemma(first, second)
    appendSameAsAddition(first, second)
  }.ensuring(_ => sumVotingPower(first) <= sumVotingPower(first ++ second))

  @opaque
  def appendSameAsAddition(@induct first: List[(Address, Validator)], second: List[(Address, Validator)]): Unit = {
  }.ensuring(_ => sumVotingPower(first ++ second) == sumVotingPower(first) + sumVotingPower(second))

  @opaque
  def additionLemma(first: List[(Address, Validator)], @induct second: List[(Address, Validator)]): Unit = {
  }.ensuring(_ => sumVotingPower(first) <= sumVotingPower(first) + sumVotingPower(second))
}