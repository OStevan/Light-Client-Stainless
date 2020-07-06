package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStoreFactory
import ch.epfl.ognjanovic.stevan.tendermint.light.LightBlockStatuses.Trusted
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.{SimpleTrustedState, TrustedState}

class DefaultTrustedStateFactory(private val lightStoreFactory: LightStoreFactory) extends TrustedStateFactory {

  override def trustedState(configuration: TrustedStateFactory.TrustedStateConfiguration): TrustedState =
    configuration match {
      case TrustedStateFactory.LightStoreBackedTrustedStateConfiguration(
            trustedLightBlock,
            votingPowerVerifier,
            lightStoreConfiguration,
            lightStore) ⇒
        val store = lightStore.getOrElse(lightStoreFactory.lightStore(lightStoreConfiguration))
        store.update(trustedLightBlock, Trusted)
        new LightStoreBackedTrustedState(store, votingPowerVerifier)

      case TrustedStateFactory.SimpleTrustedStateConfiguration(trustedLightBlock, votingPowerVerifier) ⇒
        SimpleTrustedState(trustedLightBlock, votingPowerVerifier)

      case _ ⇒ ???
    }

}
