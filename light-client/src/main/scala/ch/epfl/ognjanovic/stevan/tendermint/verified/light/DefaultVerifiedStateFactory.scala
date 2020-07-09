package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStoreFactory
import ch.epfl.ognjanovic.stevan.tendermint.light.LightBlockStatuses.Trusted
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.{SimpleVerifiedState, VerifiedState}

class DefaultVerifiedStateFactory(private val lightStoreFactory: LightStoreFactory) extends VerifiedStateFactory {

  override def verifiedState(configuration: VerifiedStateFactory.VerifiedStateConfiguration): VerifiedState =
    configuration match {
      case VerifiedStateFactory.LightStoreBackedVerifiedStateConfiguration(
            trustedLightBlock,
            votingPowerVerifier,
            lightStoreConfiguration) ⇒
        val store = lightStoreConfiguration match {
          case Left(value) ⇒ lightStoreFactory.lightStore(value)
          case Right(value) ⇒ value
        }

        store.update(trustedLightBlock, Trusted)
        new LightStoreBackedVerifiedState(store, votingPowerVerifier)

      case VerifiedStateFactory.SimpleVerifiedStateConfiguration(trustedLightBlock, votingPowerVerifier) ⇒
        SimpleVerifiedState(trustedLightBlock, votingPowerVerifier)

      case _ ⇒ ???
    }

}
