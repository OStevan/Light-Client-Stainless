package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.light.MultiStepVerifierFactories.MultiStepVerifierFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height, LightBlock}
import stainless.lang

import scala.concurrent.Channel

/**
 * Not a real implementation just a sketch of how it should look like
 */
class Supervisor(
  private val primary: LightBlockProvider,
  private val witnesses: List[LightBlockProvider],
  private val votingPowerVerifier: VotingPowerVerifier,
  private val verifierBuilder: MultiStepVerifierFactory,
  private val trustDuration: Duration,
  private var trustedState: TrustedState,
  private val channel: Channel[Option[Long]])
    extends Runnable {
  // TODO add fork detector and evidence reporter

  // TODO probably have to change error
  def syncToHighest(): Either[LightBlock, VerificationError] = {
    sync(None)
  }

  def syncToHeight(height: Height): Either[LightBlock, VerificationError] = {
    sync(Option(height))
  }

  private def sync(height: Option[Height]): Either[LightBlock, VerificationError] = {
    // TODO introduce primary changes
    val primaryVerifier = verifierBuilder.constructVerifier(primary, votingPowerVerifier, trustDuration)

    val primaryResult =
      primaryVerifier.verifyUntrusted(trustedState, UntrustedStates.empty(height.getOrElse(primary.currentHeight)))

    primaryResult.outcome match {
      case lang.Left(_) ⇒
        detectForks(primaryResult.trustedState.trustedLightBlock, witnesses)
        ???
      case lang.Right(content) ⇒ Right[LightBlock, VerificationError](content)
    }
  }

  private def detectForks(block: LightBlock, providers: List[LightBlockProvider]): Unit = ???

  override def run(): Unit = {
    while (true) {
      val height = channel.read
      height match {
        case Some(value) if value > 0 ⇒ syncToHeight(Height(value))
        case Some(_) ⇒ return
        case None ⇒ syncToHighest()
      }
    }
  }

}
