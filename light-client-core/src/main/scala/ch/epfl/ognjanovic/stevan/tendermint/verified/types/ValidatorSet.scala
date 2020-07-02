package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Validators.{InfoHashable, Validator}
import stainless.annotation._
import stainless.collection._
import stainless.lang._
import utils._
import utils.ListSetUtils._

case class ValidatorSet(totalPower: VotingPower, powerAssignments: ListMap[Address, Validator]) {
  require(
    powerAssignments.forall(value => value._2.votingPower.isPositive) &&
      totalPower == powerAssignments.toList.foldLeft(VotingPower(0))((acc, value) => acc + value._2.votingPower) &&
      !powerAssignments.isEmpty)

  @pure
  @extern
  def keys: Set[Address] = {
    powerAssignments.toList.map(_._1).content
  }.ensuring(res => res.nonEmpty)

  def values: List[Validator] = powerAssignments.toList.map(_._2)

  def contains(node: Address): Boolean = keys.contains(node)

  def nodesPower(nodes: List[Address]): VotingPower =
    ValidatorSet.sumVotingPower(powerAssignments.toList.filter(pair => nodes.contains(pair._1)))

  def toInfoHashable: List[InfoHashable] = values.map(p => p.toInfoHashable)
}

object ValidatorSet {

  @pure
  def sumVotingPower(votingPowers: List[(Address, Validator)]): VotingPower = votingPowers match {
    case Nil() => VotingPower(0)
    case Cons(head, tail) => head._2.votingPower + sumVotingPower(tail)
  }

  @opaque
  @ghost
  def correctLemma(
    firstCorrect: VotingPower,
    firstFaulty: VotingPower,
    secondCorrect: VotingPower,
    secondFaulty: VotingPower): Unit = {
    require(firstCorrect >= secondCorrect && firstFaulty <= secondFaulty)
  }.ensuring(_ => !(firstCorrect > firstFaulty * VotingPower(2)) ==> !(secondCorrect > secondFaulty * VotingPower(2)))

  @opaque
  @ghost
  def subsetPowerLemma(first: List[Address], second: List[Address], validatorSet: ValidatorSet): Unit = {
    require(
      first.forall(second.contains) &&
        second.forall(validatorSet.powerAssignments.toList.map(_._1).contains) &&
        ListOps.noDuplicate(first) &&
        ListOps.noDuplicate(second)
    )
    transitivityLemma(first, second, validatorSet.powerAssignments.toList.map(_._1))
    val firstFiltered = validatorSet.powerAssignments.toList.filter(node => first.contains(node._1))
    uniquenessTransitivity(validatorSet.powerAssignments.toList)
    filteringPreservesPredicate(first, validatorSet.powerAssignments.toList)

    val secondFiltered = validatorSet.powerAssignments.toList.filter(node => second.contains(node._1))
    filteringPreservesPredicate(second, validatorSet.powerAssignments.toList)

    subsetFilteringCreatesSubsets(first, second, validatorSet.powerAssignments.toList)

    subsetSumLessEq(firstFiltered, secondFiltered)
  }.ensuring(_ => validatorSet.nodesPower(first) <= validatorSet.nodesPower(second))

  @opaque
  @ghost
  def subsetSumLessEq(@induct first: List[(Address, Validator)], second: List[(Address, Validator)]): Unit = {
    require(first.forall(second.contains) && ListOps.noDuplicate(first) && ListOps.noDuplicate(second))
    val difference = removingFromSet(second, first)
    sumWithDifferenceIsEqual(first, second)
    appendSameAsAddition(first, difference)
    appendIncreases(first, difference)
  }.ensuring(_ => sumVotingPower(first) <= sumVotingPower(second))

  @opaque
  @ghost
  def sumWithDifferenceIsEqual(first: List[(Address, Validator)], second: List[(Address, Validator)]): Unit = {
    require(first.forall(second.contains) && ListOps.noDuplicate(first) && ListOps.noDuplicate(second))
    second match {
      case Nil() => ()

      case Cons(h, t) if first.contains(h) =>
        val removed = first - h
        removeOne(h, first)
        interestingEquality(h, first, t)
        listSetRemoveHeadSameAsSubtraction(second)
        removingContainment(h, first, second)
        sumWithDifferenceIsEqual(removed, t)

      case Cons(_, t) =>
        doesNotHaveHeadContainedInTail(first, second)
        sumWithDifferenceIsEqual(first, t)
    }
  }.ensuring(_ => sumVotingPower(first) + sumVotingPower(second -- first) == sumVotingPower(second))

  @opaque
  @ghost
  def removeOne(elem: (Address, Validator), list: List[(Address, Validator)]): Unit = {
    require(ListOps.noDuplicate(list) && list.contains(elem) && list.nonEmpty)
    list match {
      case Cons(_, Nil()) => ()
      case Cons(_, tail) if !tail.contains(elem) => removingNonContained(tail, elem)
      case Cons(_, tail) => removeOne(elem, tail)
    }
  }.ensuring(_ =>
    sumVotingPower(list) == elem._2.votingPower + sumVotingPower(list - elem) && ListOps.noDuplicate(list - elem))

  @opaque
  @ghost
  def appendIncreases(first: List[(Address, Validator)], second: List[(Address, Validator)]): Unit = {
    additionLemma(first, second)
    appendSameAsAddition(first, second)
  }.ensuring(_ => sumVotingPower(first) <= sumVotingPower(first ++ second))

  @opaque
  @ghost
  def appendSameAsAddition(@induct first: List[(Address, Validator)], second: List[(Address, Validator)]): Unit = {}
    .ensuring(_ => sumVotingPower(first ++ second) == sumVotingPower(first) + sumVotingPower(second))

  @opaque
  @ghost
  def additionLemma(first: List[(Address, Validator)], @induct second: List[(Address, Validator)]): Unit = {}.ensuring(
    _ => sumVotingPower(first) <= sumVotingPower(first) + sumVotingPower(second))

}
