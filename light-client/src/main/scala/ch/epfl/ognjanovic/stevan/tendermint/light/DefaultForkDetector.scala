package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.hashing.Hashers.Hasher
import ch.epfl.ognjanovic.stevan.tendermint.light.ForkDetection._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.{MultiStepVerifier, UntrustedStates, VotingPowerVerifiers}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.SimpleTrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.ExpiredTrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.lang

sealed class DefaultForkDetector(private val hasher: Hasher) extends ForkDetector {

  override def detectForks(
    targetLightBlock: LightBlock,
    trustedLightBlock: LightBlock,
    witnesses: List[MultiStepVerifier]): ForkDetection.ForkDetectionResult = {
    val expectedHash = hasher.hashHeader(targetLightBlock.header)

    val forks = witnesses.flatMap(witness ⇒ {
      val witnessBlock = witness.lightBlockProvider.lightBlock(targetLightBlock.header.height)

      val witnessBlockHash = hasher.hashHeader(witnessBlock.header)

      if (expectedHash == witnessBlockHash) {
        return None[Fork]
      }

      // TODO this should change definitely
      val witnessVerificationResult = witness.verifyUntrusted(
        SimpleTrustedState(trustedLightBlock, VotingPowerVerifiers.defaultTrustVerifier),
        UntrustedStates.empty(targetLightBlock.header.height))

      witnessVerificationResult.outcome match {
        case lang.Left(_) ⇒ Some(Forked(targetLightBlock, witnessBlock))
        case lang.Right(content) if content == ExpiredTrustedState ⇒ Some(Forked(targetLightBlock, witnessBlock))
        case lang.Right(_) ⇒ Some(Faulty(witnessBlock))
      }
    })

    if (forks.isEmpty)
      NoForks
    else
      ForkDetected(forks)
  }

}
