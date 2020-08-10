package ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Address, ValidatorSet, VotingPower}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.ValidatorSet.{correctLemma, subsetPowerLemma}
import stainless.annotation.{extern, ghost, opaque, pure}
import stainless.lang._
import stainless.proof.check
import utils.ListSet
import utils.ListSetUtils._

case class FaultChecker() {

  @pure
  def isCorrect(validatorSet: ValidatorSet, faultyNodes: ListSet[Address]): Boolean = {
    val keySet = validatorSet.powerAssignments.toList.map(_._1)
    val faultyNodesSet = faultyNodes.toList
    val difference = removingFromSet(keySet, faultyNodesSet)
    val intersection = keySet & faultyNodesSet
    setIntersectionLemma(keySet, faultyNodesSet)
    validatorSet.nodesPower(difference) > validatorSet.nodesPower(intersection) * VotingPower(2)
  }

}

@ghost
object FaultChecker {

  @opaque
  def moreFaultyDoesNotHelp(faultChecker: FaultChecker, current: ListSet[Address], next: ListSet[Address]): Unit = {
    require(current.subsetOf(next))
  }.ensuring(_ =>
    forall((validator: ValidatorSet) => {
      faultyExpansion(faultChecker, next, current, validator)
      !faultChecker.isCorrect(validator, current) ==> !faultChecker.isCorrect(validator, next)
    }))

  // TODO if isCorrect is inlined this lemma can be proved but calls to isCorrect at other locations fail
  @extern
  def faultyExpansion(
    faultChecker: FaultChecker,
    next: ListSet[Address],
    current: ListSet[Address],
    validatorSet: ValidatorSet): Unit = {
    require(current.subsetOf(next))
    val keys = validatorSet.powerAssignments.toList.map(_._1)
    val nextList = listSetSubsetEquivalence(next)
    val currentList = listSetSubsetEquivalence(current)
    val currentDiff = removingFromSet(keys, currentList)
    val nextDiff = removingFromSet(keys, nextList)

    val difference_proof = {
      expandPredicate(currentList, currentList.contains, nextList.contains)
      subsetRemovingLemma(keys, currentList, nextList)
      subsetPowerLemma(nextDiff, currentDiff, validatorSet)
      check(validatorSet.nodesPower(currentDiff) >= validatorSet.nodesPower(nextDiff))
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
      subsetPowerLemma(currentIntersection, nextIntersection, validatorSet)
      check(validatorSet.nodesPower(currentIntersection) <= validatorSet.nodesPower(nextIntersection))
    }

    correctLemma(
      validatorSet.nodesPower(currentDiff),
      validatorSet.nodesPower(currentIntersection),
      validatorSet.nodesPower(nextDiff),
      validatorSet.nodesPower(nextIntersection)
    )

    assert(
      !(validatorSet.nodesPower(currentDiff) > validatorSet.nodesPower(currentIntersection) * VotingPower(2)) ==>
        !(validatorSet.nodesPower(nextDiff) > validatorSet.nodesPower(nextIntersection) * VotingPower(2)))
    assert(
      faultChecker.isCorrect(validatorSet, current) ==
        validatorSet.nodesPower(currentDiff) > validatorSet.nodesPower(currentIntersection) * VotingPower(2))
    assert(
      faultChecker.isCorrect(validatorSet, next) ==
        validatorSet.nodesPower(nextDiff) > validatorSet.nodesPower(nextIntersection) * VotingPower(2))
  }.ensuring(_ => !faultChecker.isCorrect(validatorSet, current) ==> !faultChecker.isCorrect(validatorSet, next))

}
