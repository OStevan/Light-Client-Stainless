package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.pure

object VerificationTraces {

  abstract class VerificationTrace {

    @pure
    def isTrusted(lightBlock: LightBlock): Boolean = {
      require(lightBlock.header.height > currentHeight())
      ??? : Boolean
    }

    @pure
    def verified: LightBlock

    /**
     * Tries to "improve" the current verified state with addition of a new signed header of a greater height.
     *
     * @param lightBlock which will be the new verified header if it can be trusted
     * @return new verified state
     */
    @pure
    def increaseTrust(lightBlock: LightBlock): VerificationTrace = {
      require(lightBlock.header.height > currentHeight() && isTrusted(lightBlock))
      ??? : VerificationTrace
    }.ensuring(res => res.currentHeight() > currentHeight() && res.currentHeight() == lightBlock.header.height)

    /**
     * The height of the last block that we trust.
     */
    @pure
    def currentHeight(): Height = {
      verified.header.height
    }.ensuring(res => res == verified.header.height)

    @pure
    def isAdjacent(lightBlock: LightBlock): Boolean = {
      require(currentHeight() < lightBlock.header.height)
      lightBlock.header.height == verified.header.height + 1
    }.ensuring(res =>
      (res && currentHeight() + 1 == lightBlock.header.height) ||
        (!res && currentHeight() + 1 < lightBlock.header.height))

  }

  case class SimpleVerificationTrace(verified: LightBlock, trustVerifier: VotingPowerVerifier) extends VerificationTrace {

    @pure
    override def currentHeight(): Height = verified.header.height

    @pure
    override def increaseTrust(lightBlock: LightBlock): VerificationTrace = {
      require(lightBlock.header.height > this.verified.header.height && isTrusted(lightBlock))
      SimpleVerificationTrace(lightBlock, trustVerifier)
    }.ensuring(res => res.currentHeight() > currentHeight() && res.currentHeight() == lightBlock.header.height)

    @pure
    override def isTrusted(lightBlock: LightBlock): Boolean = {
      require(lightBlock.header.height > currentHeight())
      if (isAdjacent(lightBlock))
        internalAdjacentHeaderTrust(lightBlock)
      else
        internalNonAdjacentHeaderTrust(lightBlock)
    }

    @pure
    private def internalAdjacentHeaderTrust(lightBlock: LightBlock): Boolean = {
      require(currentHeight() < lightBlock.header.height && isAdjacent(lightBlock))
      verified.nextValidatorSet.toInfoHashable == lightBlock.validatorSet.toInfoHashable
    }

    @pure
    private def internalNonAdjacentHeaderTrust(lightBlock: LightBlock): Boolean = {
      require(currentHeight() < lightBlock.header.height && !isAdjacent(lightBlock))

      trustVerifier.trustedCommit(verified.nextValidatorSet, lightBlock.commit)
    }

  }

}
