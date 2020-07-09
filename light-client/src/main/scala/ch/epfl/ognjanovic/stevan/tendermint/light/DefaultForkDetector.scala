package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.hashing.Hashers.Hasher
import ch.epfl.ognjanovic.stevan.tendermint.light.ForkDetection._
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.MultiStepVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedStates.UntrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationErrors.ExpiredTrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock, PeerId}
import stainless.lang

sealed class DefaultForkDetector(private val hasher: Hasher, private val untrustedStateSupplier: Height ⇒ UntrustedState)
    extends ForkDetector {

  override def detectForks(
    trustedStateSupplier: PeerId ⇒ TrustedState,
    targetLightBlock: LightBlock,
    witnesses: List[MultiStepVerifier]): ForkDetection.ForkDetectionResult = {
    val expectedHash = hasher.hashHeader(targetLightBlock.header)

    val forks: List[Fork] = witnesses.flatMap(witness ⇒ {
      val witnessBlock = witness.lightBlockProvider.lightBlock(targetLightBlock.header.height)

      val witnessBlockHash = hasher.hashHeader(witnessBlock.header)

      if (expectedHash == witnessBlockHash)
        Option.empty[Fork]
      else {
        val witnessVerificationResult = witness.verifyUntrusted(
          trustedStateSupplier(witnessBlock.peerId),
          untrustedStateSupplier(targetLightBlock.header.height)
        )

        witnessVerificationResult.outcome match {
          case lang.Left(_) ⇒ Some(Forked(targetLightBlock, witnessBlock))
          case lang.Right(content) if content == ExpiredTrustedState ⇒ Some(Forked(targetLightBlock, witnessBlock))
          case lang.Right(reason) ⇒ Some(Faulty(witnessBlock, reason))
        }
      }
    })

    if (forks.isEmpty)
      NoForks
    else
      ForkDetected(forks)
  }

}
