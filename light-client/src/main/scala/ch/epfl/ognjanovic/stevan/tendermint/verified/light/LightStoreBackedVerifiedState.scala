package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.light.LightBlockStatuses.{Trusted, Verified}
import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStore
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.VerifiedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock

class LightStoreBackedVerifiedState(private val lightStore: LightStore, private val trustVerifier: VotingPowerVerifier)
    extends VerifiedState {

  override def isTrusted(lightBlock: LightBlock): Boolean = {
    if (!(lightBlock.header.height > currentHeight()))
      throw new IllegalArgumentException(
        "It is not possible to call this method with a block lower that the latest verified one")

    if (isAdjacent(lightBlock))
      verified.header.nextValidators == lightBlock.header.validators
    else
      trustVerifier.trustedCommit(verified.nextValidatorSet, lightBlock.commit)
  }

  override def verified: LightBlock = {
    val possibleTrusted = lightStore.latest(Trusted)
    val possibleVerified = lightStore.latest(Verified)

    (possibleTrusted, possibleVerified) match {
      case (Some(trusted), Some(verified)) if trusted.header.height < verified.header.height ⇒
        verified
      case (Some(trusted), None) ⇒ trusted
      case _ ⇒ throw new IllegalStateException("Verified state should never end up in this situation")
    }
  }

  override def increaseTrust(lightBlock: LightBlock): VerifiedState = {
    if (!(lightBlock.header.height > currentHeight() && isTrusted(lightBlock)))
      throw new IllegalArgumentException("Illegal light block:" + lightBlock)
    lightStore.update(lightBlock, Verified)
    new LightStoreBackedVerifiedState(lightStore, trustVerifier)
  }

}
