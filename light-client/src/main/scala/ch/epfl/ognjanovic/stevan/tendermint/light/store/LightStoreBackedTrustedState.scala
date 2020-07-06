package ch.epfl.ognjanovic.stevan.tendermint.light.store

import ch.epfl.ognjanovic.stevan.tendermint.light.LightBlockStatuses.{Trusted, Verified}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock

class LightStoreBackedTrustedState(private val lightStore: LightStore, private val trustVerifier: VotingPowerVerifier)
    extends TrustedState {

  override def trusted(lightBlock: LightBlock): Boolean = {
    if (!(lightBlock.header.height > currentHeight()))
      throw new IllegalArgumentException(
        "It is not possible to call this method with a block lower that the latest verified one")

    if (isAdjacent(lightBlock))
      trustedLightBlock.header.nextValidators == lightBlock.header.validators
    else
      trustVerifier.trustedCommit(trustedLightBlock.nextValidatorSet, lightBlock.commit)
  }

  override def trustedLightBlock: LightBlock = {
    val possibleTrusted = lightStore.latest(Trusted)
    val possibleVerified = lightStore.latest(Verified)

    (possibleTrusted, possibleVerified) match {
      case (Some(trusted), Some(verified)) if trusted.header.height < verified.header.height ⇒
        verified
      case (Some(trusted), None) ⇒ trusted
      case _ ⇒ throw new IllegalStateException("Trusted state should never end up in this situation")
    }
  }

  override def increaseTrust(lightBlock: LightBlock): TrustedState = {
    if (!(lightBlock.header.height > currentHeight() && trusted(lightBlock)))
      throw new IllegalArgumentException("Illegal light block:" + lightBlock)
    lightStore.update(lightBlock, Verified)
    new LightStoreBackedTrustedState(lightStore, trustVerifier)
  }

}
