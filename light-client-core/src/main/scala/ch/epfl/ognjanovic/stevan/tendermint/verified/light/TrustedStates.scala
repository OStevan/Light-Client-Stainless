package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.pure

object TrustedStates {

  abstract class TrustedState {

    @pure
    def trusted(lightBlock: LightBlock): Boolean = {
      require(lightBlock.header.height > currentHeight())
      ??? : Boolean
    }

    @pure
    def trustedLightBlock: LightBlock

    /**
     * Tries to "improve" the current trusted state with addition of a new signed header of a greater height.
     *
     * @param lightBlock which will be the new trusted header if it can be trusted
     * @return new trusted state or the old one if the trust is not reachable
     */
    @pure
    def increaseTrust(lightBlock: LightBlock): TrustedState = {
      require(lightBlock.header.height > currentHeight() && trusted(lightBlock))
      ??? : TrustedState
    }.ensuring(res => res.currentHeight() > currentHeight() && res.currentHeight() == lightBlock.header.height)

    /**
     * The height of the last block that we trust.
     */
    @pure
    sealed def currentHeight(): Height = {
      trustedLightBlock.header.height
    }.ensuring(res => res == trustedLightBlock.header.height)

    @pure
    sealed def isAdjacent(lightBlock: LightBlock): Boolean = {
      require(currentHeight() < lightBlock.header.height)
      lightBlock.header.height == trustedLightBlock.header.height + 1
    }.ensuring(res =>
    (res && currentHeight() + 1 == lightBlock.header.height) ||
    (!res && currentHeight() + 1 < lightBlock.header.height))
  }

  case class SimpleTrustedState(trustedLightBlock: LightBlock, trustVerifier: VotingPowerVerifier)
      extends TrustedState {

    @pure
    override def currentHeight(): Height = trustedLightBlock.header.height

    @pure
    override def increaseTrust(lightBlock: LightBlock): TrustedState = {
      require(lightBlock.header.height > this.trustedLightBlock.header.height && trusted(lightBlock))
      SimpleTrustedState(lightBlock, trustVerifier)
    }.ensuring(res => res.currentHeight() > currentHeight() && res.currentHeight() == lightBlock.header.height)


    @pure
    override def trusted(lightBlock: LightBlock): Boolean = {
      require(lightBlock.header.height > currentHeight())
      if (isAdjacent(lightBlock))
        internalAdjacentHeaderTrust(lightBlock)
      else
        internalNonAdjacentHeaderTrust(lightBlock)
    }

    @pure
    private def internalAdjacentHeaderTrust(lightBlock: LightBlock): Boolean = {
      require(isAdjacent(lightBlock))
      trustedLightBlock.header.nextValidators == lightBlock.header.validators
    }

    @pure
    private def internalNonAdjacentHeaderTrust(lightBlock: LightBlock): Boolean = {
      require(lightBlock.header.height > this.trustedLightBlock.header.height && !isAdjacent(lightBlock))

      trustVerifier.trustedCommit(trustedLightBlock.nextValidatorSet, lightBlock.commit)
    }

  }

}
