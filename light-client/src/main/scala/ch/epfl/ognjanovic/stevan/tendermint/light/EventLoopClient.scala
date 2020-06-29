package ch.epfl.ognjanovic.stevan.tendermint.light

import java.util.concurrent.{ExecutorService, Executors}

import ch.epfl.ognjanovic.stevan.tendermint.light.MultiStepVerifierFactories.MultiStepVerifierFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height, LightBlock}
import stainless.lang

import scala.concurrent.{Channel, ExecutionContext, ExecutionContextExecutorService, Future}

object EventLoopClient {

  /**
   * Not a real implementation just a sketch of how it should look like
   */
  class EventLoopSupervisor(
    private val primary: LightBlockProvider,
    private val witnesses: List[LightBlockProvider],
    private val votingPowerVerifier: VotingPowerVerifier,
    private val verifierBuilder: MultiStepVerifierFactory,
    private val trustDuration: Duration,
    private var trustedState: TrustedState,
    private val channel: Channel[Option[Long]])
      extends Supervisor {
    // TODO add fork detector and evidence reporter

    // TODO probably have to change error
    override def verifyToHeight(height: Height): Either[LightBlock, VerificationError] = verifyToTarget(Some(height))

    override def verifyToHighest(): Either[LightBlock, VerificationError] = verifyToTarget(None)

    override def handle: Handle = {
      val executorContext = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
      new EventLoopHandle(this, executorContext)
    }

    private def verifyToTarget(height: Option[Height]): Either[LightBlock, VerificationError] = {
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

  }

  private class EventLoopHandle(
    private val eventLoopSupervisor: EventLoopSupervisor,
    implicit private val executorExecutionService: ExecutionContextExecutorService)
      extends Handle {

    override def verifyToHighest(): Future[Either[LightBlock, VerificationError]] = {
      Future {
        eventLoopSupervisor.verifyToHighest()
      }
    }

    override def verifyToHeight(height: Height): Future[Either[LightBlock, VerificationError]] = {
      Future {
        eventLoopSupervisor.verifyToHeight(height)
      }
    }

    override def terminate(): Unit = {
      // TODO this should be improved as soon as possible
      executorExecutionService.shutdown()
    }

  }

}
