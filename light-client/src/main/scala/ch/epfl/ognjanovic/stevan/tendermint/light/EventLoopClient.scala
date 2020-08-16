package ch.epfl.ognjanovic.stevan.tendermint.light

import java.util.concurrent.{Executors, TimeUnit}

import ch.epfl.ognjanovic.stevan.tendermint.light.ForkDetection.{ForkDetector, Forked}
import ch.epfl.ognjanovic.stevan.tendermint.light.Supervisor._
import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStore
import ch.epfl.ognjanovic.stevan.tendermint.light.LightBlockStatuses.Trusted
import ch.epfl.ognjanovic.stevan.tendermint.verified.fork.{PeerList ⇒ GenericPeerList}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.FetchedStacks.FetchedStack
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.MultiStepVerifierFactories.MultiStepVerifierFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TimeValidatorFactories.TimeValidatorConfig
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationTraces.VerificationTrace
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
    private val fetchedStackSupplier: Height ⇒ FetchedStack,
    private val timeValidatorConfig: TimeValidatorConfig,
    private val lightStore: LightStore,
    private val forkDetector: ForkDetector,
    private val primaryStateSupplier: (LightBlock, VotingPowerVerifier) ⇒ (() ⇒ Iterable[LightBlock], VerificationTrace),
    private val witnessVerifiedStateSupplier: (LightBlock, VotingPowerVerifier) ⇒ PeerId ⇒ VerificationTrace)
      extends Supervisor {

    // force sharing between constructed handles, when there is a need for multiple threads to use the same supervisor
    // recommended approach is to wrap the Handle to prevent unwanted closing
    private lazy val executorService = ExecutionContext.fromExecutorService(Executors.newSingleThreadExecutor())

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
      new EventLoopHandle(this, executorService)
    }

    @tailrec
    private def verifyToTarget(
      height: Option[Height],
      peerList: PeerList): (PeerList, Either[LightBlock, Supervisor.Error]) = {
      val primaryVerifier =
        verifierBuilder.constructVerifier(peerList.primary, votingPowerVerifier, timeValidatorConfig)

      val trustedLightBlock = lightStore.latest(Trusted)

      if (trustedLightBlock.isEmpty)
        return (peerList, Right(NoVerifiedState))

      val (verificationCollector, primaryVerifiedState) =
        primaryStateSupplier(trustedLightBlock.get, votingPowerVerifier)

      val primaryResult =
        primaryVerifier.verifyUntrusted(
          primaryVerifiedState,
          fetchedStackSupplier(height.getOrElse(peerList.primary.currentHeight)))

      primaryResult.outcome match {
        case lang.Left(_) ⇒
          if (peerList.witnessesIds.isEmpty)
            return (peerList, Right(NoWitnesses))

          val forkDetectionResult = forkDetector.detectForks(
            witnessVerifiedStateSupplier(trustedLightBlock.get, votingPowerVerifier),
            primaryResult.verificationTrace.verified,
            peerList.witnesses
              .map(verifierBuilder.constructVerifier(_, votingPowerVerifier, timeValidatorConfig))
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
              (peerList, Left(primaryResult.verificationTrace.verified))
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
      // reject incoming tasks
      executorExecutionService.shutdown()
      try {
        // await termination of currently submitted tests
        if (!executorExecutionService.awaitTermination(60, TimeUnit.SECONDS)) {
          // force cancellation of currently submitted tasks
          executorExecutionService.shutdownNow()
          // wait a while for canceled tasks to finish
          if (!executorExecutionService.awaitTermination(60, TimeUnit.SECONDS)) {
            System.err.println("Supervisor did not terminate")
          }
        }
      } catch {
        // can happen on await that current thread is also interrupted so force shutdown and keep the signal
        case _: InterruptedException ⇒
          executorExecutionService.shutdownNow()
          Thread.currentThread().interrupt()
      }
    }

  }

}
