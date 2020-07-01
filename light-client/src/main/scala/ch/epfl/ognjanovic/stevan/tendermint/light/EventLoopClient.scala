package ch.epfl.ognjanovic.stevan.tendermint.light

import java.util.concurrent.Executors

import ch.epfl.ognjanovic.stevan.tendermint.light.ForkDetection.ForkDetector
import ch.epfl.ognjanovic.stevan.tendermint.light.MultiStepVerifierFactories.MultiStepVerifierFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.fork.PeerList
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height, LightBlock}
import stainless.lang

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

object EventLoopClient {

  /**
   * Not a real implementation just a sketch of how it should look like
   */
  class EventLoopSupervisor(
    @volatile private var peerList: PeerList,
    private val votingPowerVerifier: VotingPowerVerifier,
    private val verifierBuilder: MultiStepVerifierFactory,
    private val trustDuration: Duration,
    private var trustedState: TrustedState, // currently var, but should be changed with store and recreate a trusted state for verification
    private val forkDetector: ForkDetector)
      extends Supervisor {
    // TODO add fork detector and evidence reporter

    // TODO probably have to change error
    override def verifyToHeight(height: Height): Either[LightBlock, Supervisor.Error] = verifyToTarget(Some(height))

    override def verifyToHighest(): Either[LightBlock, Supervisor.Error] = verifyToTarget(None)

    override def handle: Handle = {
      val executorContext = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
      new EventLoopHandle(this, executorContext)
    }

    private def verifyToTarget(height: Option[Height]): Either[LightBlock, Supervisor.Error] = {
      // TODO introduce primary changes, with concurrency in mind???
      val primaryVerifier = verifierBuilder.constructVerifier(peerList.primary, votingPowerVerifier, trustDuration)

      val primaryResult =
        primaryVerifier.verifyUntrusted(
          trustedState,
          UntrustedStates.empty(height.getOrElse(peerList.primary.currentHeight)))

      primaryResult.outcome match {
        case lang.Left(_) ⇒
          val forkDetectionResult = forkDetector.detectForks(
            primaryResult.trustedState.trustedLightBlock,
            trustedState.trustedLightBlock,
            peerList.witnesses.map(verifierBuilder.constructVerifier(_, votingPowerVerifier, trustDuration)).toScala
          )

          forkDetectionResult match {
            // TODO report forks and change witness
            case ForkDetection.ForkDetected(detected) ⇒
              Right(new Supervisor.Error() {
                override def toString: String = "Error:" + forkDetectionResult.toString
              })
            case ForkDetection.NoForks ⇒
              trustedState = primaryResult.trustedState
              Left(primaryResult.trustedState.trustedLightBlock)
          }
        case lang.Right(content) ⇒
          // TODO change primary
          Right(
            new Supervisor.Error {
              override def toString: String = "Error:" + content.toString
            }
          )
      }
    }

  }

  private class EventLoopHandle(
    private val eventLoopSupervisor: EventLoopSupervisor,
    implicit private val executorExecutionService: ExecutionContextExecutorService)
      extends Handle {

    override def verifyToHighest(): Future[Either[LightBlock, Supervisor.Error]] = {
      Future {
        eventLoopSupervisor.verifyToHighest()
      }
    }

    override def verifyToHeight(height: Height): Future[Either[LightBlock, Supervisor.Error]] = {
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
