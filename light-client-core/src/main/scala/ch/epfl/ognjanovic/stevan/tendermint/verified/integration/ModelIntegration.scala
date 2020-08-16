package ch.epfl.ognjanovic.stevan.tendermint.verified.integration

import ch.epfl.ognjanovic.stevan.tendermint.verified.blockchain.BlockchainStates.BlockchainState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.CommitValidators.DefaultCommitValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.FetchedStacks.InMemoryFetchedStack
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockValidators.DummyLightBlockValidator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustVerifiers.DefaultTrustVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.VerificationError
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationTraces.{
  StartingVerificationTrace,
  VerificationTrace
}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types._
import stainless.annotation.{extern, pure}
import stainless.collection._
import stainless.lang._

object ModelIntegration {

  def snapshotExecution(
    blockchainState: BlockchainState,
    trustedHeight: Height,
    heightToVerify: Height,
    nextHeightCalculator: NextHeightCalculator
  ): Either[Unit, VerificationError] = {
    require(
      blockchainState.currentHeight() >= Height(2) &&
        blockchainState.currentHeight() > heightToVerify &&
        heightToVerify > trustedHeight)
    val soundSignedHeaderProvider = BlockchainLightBlockProviders(blockchainState)
    val trustedSignedHeader = soundSignedHeaderProvider.lightBlock(trustedHeight)

    val verificationTrace: VerificationTrace =
      StartingVerificationTrace(trustedSignedHeader, VotingPowerVerifiers.defaultVotingPowerVerifier)
    val fetchedStack = InMemoryFetchedStack(heightToVerify, List.empty)
    assert(fetchedStack.peek().forall(heightToVerify < _.header.height))
    assert(verificationTrace.currentHeight() < heightToVerify)
    assert(heightToVerify <= fetchedStack.targetLimit)

    val lightBlockVerifier = DefaultTrustVerifier()
    MultiStepVerifier(
      soundSignedHeaderProvider,
      Verifier(
        DummyLightBlockValidator(),
        lightBlockVerifier,
        DefaultCommitValidator(VotingPowerVerifiers.defaultVotingPowerVerifier, DummyCommitSignatureVerifier())),
      nextHeightCalculator
    )
      .verifyUntrusted(verificationTrace, fetchedStack)
      .outcome
  }

  private[integration] case class DummyCommitSignatureVerifier() extends CommitSignatureVerifier {
    override def verifyCommitSignatures(lightBlock: LightBlock): Boolean = true
  }

  private[integration] case class HeightBasedTimeValidator(height: Height) extends TimeValidator {
    override def isExpired(lightBlock: LightBlock): Boolean = height > lightBlock.header.height
    override def fromFuture(lightBlock: LightBlock): Boolean = false
  }

  private[integration] case class BlockchainLightBlockProviders(blockchainState: BlockchainState)
      extends LightBlockProvider {
    require(blockchainState.currentHeight() >= Height(2))

    @pure
    override def lightBlock(height: Height): LightBlock = {
      require(height <= currentHeight)
      blockchainState.lightBlock(height)
    }

    @pure
    override def currentHeight: Height = {
      Height(blockchainState.currentHeight().value - 1)
    }

    override def chainId: String = "verified-chain-01"

    override def latestLightBlock: LightBlock = blockchainState.lightBlock(currentHeight)

    @extern
    override def peerId: PeerId = PeerId(Key("ignore", "ingnore".getBytes()))

  }

}
