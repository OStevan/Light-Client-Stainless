package ch.epfl.ognjanovic.stevan.tendermint.light

import java.util.concurrent.Executors

import ch.epfl.ognjanovic.stevan.tendermint.light.ForkDetection.{ForkDetector, Forked}
import ch.epfl.ognjanovic.stevan.tendermint.light.Supervisor._
import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStore
import ch.epfl.ognjanovic.stevan.tendermint.light.LightBlockStatuses.Trusted
import ch.epfl.ognjanovic.stevan.tendermint.verified.fork.{PeerList ⇒ GenericPeerList}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.ExpirationCheckerFactories.ExpirationCheckerConfiguration
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.MultiStepVerifierFactories.MultiStepVerifierFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.UntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock, PeerId}
import stainless.lang

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

object EventLoopClient {

  type PeerList = GenericPeerList[PeerId, LightBlockProvider]

  // TODO add evidence reporter
  class EventLoopSupervisor(
    @volatile private var peerList: PeerList,
    private val votingPowerVerifier: VotingPowerVerifier,
    private val verifierBuilder: MultiStepVerifierFactory,
    private val untrustedStateSupplier: Height ⇒ UntrustedState,
    private val expirationCheckerConfiguration: ExpirationCheckerConfiguration,
    private val lightStore: LightStore,
    private val forkDetector: ForkDetector,
    private val primaryStateSupplier: (LightBlock, VotingPowerVerifier) ⇒ (() ⇒ Iterable[LightBlock], TrustedState),
    private val witnessTrustedStateSupplier: (LightBlock, VotingPowerVerifier) ⇒ PeerId ⇒ TrustedState)
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
      val primaryVerifier =
        verifierBuilder.constructVerifier(peerList.primary, votingPowerVerifier, expirationCheckerConfiguration)

      val trustedLightBlock = lightStore.latest(Trusted)

      if (trustedLightBlock.isEmpty)
        return (peerList, Right(NoTrustedState))

      val (verificationCollector, primaryTrustedState) = primaryStateSupplier(trustedLightBlock.get, votingPowerVerifier)

      val primaryResult =
        primaryVerifier.verifyUntrusted(
          primaryTrustedState,
          untrustedStateSupplier(height.getOrElse(peerList.primary.currentHeight)))

      primaryResult.outcome match {
        case lang.Left(_) ⇒
          if (peerList.witnessesIds.isEmpty)
            return (peerList, Right(NoWitnesses))

          val forkDetectionResult = forkDetector.detectForks(
            witnessTrustedStateSupplier(trustedLightBlock.get, votingPowerVerifier),
            primaryResult.trustedState.trustedLightBlock,
            peerList.witnesses
              .map(verifierBuilder.constructVerifier(_, votingPowerVerifier, expirationCheckerConfiguration))
              .toScala
          )

          forkDetectionResult match {
            case ForkDetection.ForkDetected(detected) ⇒
              val (forks, newPeerList) = processForks(detected)

              if (forks.nonEmpty)
                (newPeerList, Right(ForkDetected(forks.map(_.witness.peerId))))
              else
                verifyToTarget(height, newPeerList)

            case ForkDetection.NoForks ⇒
              verificationCollector().foreach(lightStore.update(_, Trusted))
              (peerList, Left(primaryResult.trustedState.trustedLightBlock))
          }
        case lang.Right(_) ⇒
          if (peerList.witnessesIds.isEmpty)
            (peerList, Right(NoPrimary))
          else
            verifyToTarget(height, peerList.markPrimaryAsFaulty)
      }
    }

    private def processForks(detected: List[ForkDetection.Fork]): (List[ForkDetection.Forked], PeerList) = {
      var resultingPeerList = peerList

      detected.foreach {
        case ForkDetection.Faulty(block, _) ⇒
          // TODO might raise an exception if the PeerId is wrong, should be fixed
          resultingPeerList = resultingPeerList.markWitnessAsFaulty(block.peerId)
        case ForkDetection.Forked(_, _) ⇒
        // TODO report forks
      }

      (detected.filter(_.isInstanceOf[Forked]).asInstanceOf[List[ForkDetection.Forked]], resultingPeerList)
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
