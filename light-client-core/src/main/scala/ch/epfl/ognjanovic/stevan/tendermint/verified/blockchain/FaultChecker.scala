package ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Address, ValidatorSet, VotingPower}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.ValidatorSet.{correctLemma, subsetPowerLemma}
import stainless.annotation.{ghost, opaque, pure}
import stainless.lang._
import stainless.proof.check
import utils.ListSet
import utils.ListSetUtils._

case class FaultChecker() {

  @pure
  def isCorrect(validatorSet: ValidatorSet, faultyNodes: ListSet[Address]): Boolean = {
    val keySet = ListSet(validatorSet.powerAssignments.toList.map(_._1))
    val difference = keySet -- faultyNodes
    val intersection = keySet & faultyNodes
    validatorSet.nodesPower(difference.toList) > validatorSet.nodesPower(intersection.toList) * VotingPower(2)
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

  @opaque
  def faultyExpansion(
    faultChecker: FaultChecker,
    next: ListSet[Address],
    current: ListSet[Address],
    validatorSet: ValidatorSet): Unit = {
    require(current.subsetOf(next))
    val keys = ListSet(validatorSet.powerAssignments.toList.map(_._1))
    val currentDiff = keys -- current
    val nextDiff = keys -- next

    val difference_proof = {
      subsetRemovingLemma(keys.toList, current.toList, next.toList)
      subsetPowerLemma(nextDiff.toList, currentDiff.toList, validatorSet)
      check(validatorSet.nodesPower(currentDiff.toList) >= validatorSet.nodesPower(nextDiff.toList))
    }

    val nextIntersection = keys & next
    val currentIntersection = keys & current
    val intersection_proof = {
      setIntersectionContainmentLemma(keys.toList, current.toList, next.toList)
      subsetPowerLemma(currentIntersection.toList, nextIntersection.toList, validatorSet)
      check(validatorSet.nodesPower(currentIntersection.toList) <= validatorSet.nodesPower(nextIntersection.toList))
    }

    correctLemma(
      validatorSet.nodesPower(currentDiff.toList),
      validatorSet.nodesPower(currentIntersection.toList),
      validatorSet.nodesPower(nextDiff.toList),
      validatorSet.nodesPower(nextIntersection.toList)
    )

    assert(
      !(validatorSet.nodesPower(currentDiff.toList) > validatorSet.nodesPower(currentIntersection.toList) * VotingPower(
        2)) ==>
        !(validatorSet.nodesPower(nextDiff.toList) > validatorSet.nodesPower(nextIntersection.toList) * VotingPower(2)))
    assert(faultChecker.isCorrect(validatorSet, current) ==
      validatorSet.nodesPower(currentDiff.toList) > validatorSet.nodesPower(currentIntersection.toList) * VotingPower(2))
    assert(
      faultChecker.isCorrect(validatorSet, next) ==
        validatorSet.nodesPower(nextDiff.toList) > validatorSet.nodesPower(nextIntersection.toList) * VotingPower(2))
  }.ensuring(_ => !faultChecker.isCorrect(validatorSet, current) ==> !faultChecker.isCorrect(validatorSet, next))

}
