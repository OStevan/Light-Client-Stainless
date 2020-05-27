package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationOutcomes._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.inlineInvariant
import stainless.collection._

object VerifierStates {

  @inline
  def untrustedStateHeightInvariant(height: Height, untrustedState: UntrustedState): Boolean = {
    untrustedState.pending match {
      case list: Cons[LightBlock] => height < list.head.header.height
      case _: Nil[LightBlock] => true
    }
  }

  @inline
  def targetHeightInvariant(targetHeight: Height, untrustedState: List[LightBlock]): Boolean = {
    untrustedState.forall(_.header.height <= targetHeight)
  }

  sealed abstract class VerifierState

  @inlineInvariant
  case class Finished(
    outcome: VerificationOutcome,
    trustedState: TrustedState,
    untrustedState: UntrustedState) extends VerifierState {
    require(
      (outcome == Success && untrustedState.pending.isEmpty) ||
        (outcome != Success && untrustedState.pending.nonEmpty))
  }

  @inlineInvariant
  case class WaitingForHeader(
    requestHeight: Height,
    targetHeight: Height,
    trustedState: TrustedState,
    untrustedState: UntrustedState) extends VerifierState {
    require(
      requestHeight <= targetHeight &&
        trustedState.currentHeight() < requestHeight &&
        untrustedStateHeightInvariant(requestHeight, untrustedState) &&
        targetHeightInvariant(targetHeight, untrustedState.pending))
  }

}
