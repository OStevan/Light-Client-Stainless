package ch.epfl.ognjanovic.stevan.tendermint.light

import java.util.concurrent.Executors

import ch.epfl.ognjanovic.stevan.tendermint.light.ForkDetection.{ForkDetector, Forked}
import ch.epfl.ognjanovic.stevan.tendermint.light.MultiStepVerifierFactories.MultiStepVerifierFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.fork.{PeerList ⇒ GenericPeerList}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height, LightBlock, PeerId}
import stainless.lang

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

object EventLoopClient {

  type PeerList = GenericPeerList[PeerId, LightBlockProvider]

  // TODO add evidence reporter, implement real errors for supervisor
  class EventLoopSupervisor(
    @volatile private var peerList: PeerList,
    private val votingPowerVerifier: VotingPowerVerifier,
    private val verifierBuilder: MultiStepVerifierFactory,
    private val trustDuration: Duration,
    private var trustedState: TrustedState, // currently var, but should be changed with store and recreate a trusted state for verification
    private val forkDetector: ForkDetector)
      extends Supervisor {

    override def verifyToHeight(height: Height): Either[LightBlock, Supervisor.Error] = {
      val (newPeerList, result) = verifyToTarget(Some(height), peerList)
      peerList = newPeerList
      result
    }

    override def verifyToHighest(): Either[LightBlock, Supervisor.Error] = {
      val (newPeerList, result) = verifyToTarget(None, peerList)
      peerList = newPeerList
      result
    }

    override def handle: Handle = {
      val executorContext = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())
      new EventLoopHandle(this, executorContext)
    }

    @tailrec
    private def verifyToTarget(
      height: Option[Height],
      peerList: PeerList): (PeerList, Either[LightBlock, Supervisor.Error]) = {
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
            case ForkDetection.ForkDetected(detected) ⇒
              val (forks, newPeerList) = processForks(detected)

              if (forks.nonEmpty)
                (
                  newPeerList,
                  Right(new Supervisor.Error() {
                    override def toString: String = "Error:" + forkDetectionResult.toString
                  })
                )
              else
                verifyToTarget(height, newPeerList)

            case ForkDetection.NoForks ⇒
              trustedState = primaryResult.trustedState
              (peerList, Left(primaryResult.trustedState.trustedLightBlock))
          }
        case lang.Right(content) ⇒
          if (peerList.witnessesIds.isEmpty)
            (
              peerList,
              Right(new Supervisor.Error() {
                override def toString: String = "Error:" + content.toString
              })
            )
          else
            verifyToTarget(height, peerList.markPrimaryAsFaulty)
      }
    }

    private def processForks(detected: List[ForkDetection.Fork]): (List[ForkDetection.Forked], PeerList) = {
      var resultingPeerList = peerList

      detected.foreach {
        case ForkDetection.Faulty(block) ⇒
          // TODO might raise an exception if the PeerId is wrong, should be fixed
          resultingPeerList = resultingPeerList.markWitnessAsFaulty(block.peerId)
        case ForkDetection.Forked(_, _) ⇒
        // TODO report forks
      }

      (detected.filter(_.isInstanceOf[Forked]).asInstanceOf, resultingPeerList)
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
