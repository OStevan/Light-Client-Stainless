package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import stainless.annotation.{opaque, pure}
import stainless.lang._
import stainless.lang.StaticChecks.Ensuring

object LightClientLemmas {

  @pure
  def terminationMeasure(waitingForHeader: WaitingForHeader): (BigInt, BigInt) = {
    val res: (BigInt, BigInt) = (
      waitingForHeader.fetchedStack.targetLimit.value - waitingForHeader.verificationTrace.currentHeight().value,
      waitingForHeader.requestHeight.value - waitingForHeader.verificationTrace.currentHeight().value
    )
    res
  }.ensuring(res => res._1 >= BigInt(0) && res._2 >= BigInt(0))

  @opaque
  def sameVerifiedStateTerminationMeasure(previous: WaitingForHeader, current: WaitingForHeader): Unit = {
    require(
      previous.fetchedStack.targetLimit == current.fetchedStack.targetLimit &&
        previous.verificationTrace.currentHeight() == current.verificationTrace.currentHeight() &&
        previous.requestHeight > current.requestHeight)
  }.ensuring { _ =>
    val previousTerminationMeasure = terminationMeasure(previous)
    val currentTerminationMeasure = terminationMeasure(current)

    previousTerminationMeasure._1 == currentTerminationMeasure._1 &&
    previousTerminationMeasure._2 > currentTerminationMeasure._2
  }

  @opaque
  def improvedVerifiedStateLemma(previous: WaitingForHeader, current: WaitingForHeader): Unit = {
    require(
      previous.fetchedStack.targetLimit == current.fetchedStack.targetLimit &&
        previous.verificationTrace.currentHeight() < current.verificationTrace.currentHeight())
  }.ensuring { _ =>
    val previousTerminationMeasure = terminationMeasure(previous)
    val currentTerminationMeasure = terminationMeasure(current)

    previousTerminationMeasure._1 > currentTerminationMeasure._1
  }

}
