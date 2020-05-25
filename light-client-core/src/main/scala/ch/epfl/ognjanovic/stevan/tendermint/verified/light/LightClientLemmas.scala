package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierStates._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import stainless.annotation.{opaque, pure}
import stainless.collection._
import stainless.lang.StaticChecks.Ensuring
import stainless.lang._

object LightClientLemmas {

  @opaque
  def transitivityOfTargetHeight(targetHeight: Height, untrustedState: UntrustedState): Unit = {
    require(targetHeightInvariant(targetHeight, untrustedState.pending))
    untrustedState.pending match {
      case Nil() => ()
      case Cons(_, t) => transitivityOfTargetHeight(targetHeight, UntrustedState(t))
    }
  }.ensuring(_ => untrustedState.pending.forall(_.header.height <= targetHeight))

  @pure
  def terminationMeasure(waitingForHeader: WaitingForHeader): (BigInt, BigInt) = {
    val res: (BigInt, BigInt) = (
      waitingForHeader.targetHeight.value - waitingForHeader.trustedState.currentHeight().value,
      waitingForHeader.requestHeight.value - waitingForHeader.trustedState.currentHeight().value
    )
    res
  }.ensuring(res => res._1 >= BigInt(0) && res._2 >= BigInt(0))

  @opaque
  def sameTrustedStateTerminationMeasure(previous: WaitingForHeader, current: WaitingForHeader): Unit = {
    require(previous.targetHeight == current.targetHeight &&
      previous.trustedState.currentHeight() == current.trustedState.currentHeight() &&
      previous.requestHeight > current.requestHeight
    )
  }.ensuring { _ =>
    val previousTerminationMeasure = terminationMeasure(previous)
    val currentTerminationMeasure = terminationMeasure(current)

    previousTerminationMeasure._1 == currentTerminationMeasure._1 &&
      previousTerminationMeasure._2 > currentTerminationMeasure._2
  }

  @opaque
  def improvedTrustedStateLemma(previous: WaitingForHeader, current: WaitingForHeader): Unit = {
    require(previous.targetHeight == current.targetHeight &&
      previous.trustedState.currentHeight() < current.trustedState.currentHeight()
    )
  }.ensuring { _ =>
    val previousTerminationMeasure = terminationMeasure(previous)
    val currentTerminationMeasure = terminationMeasure(current)

    previousTerminationMeasure._1 > currentTerminationMeasure._1
  }
}
