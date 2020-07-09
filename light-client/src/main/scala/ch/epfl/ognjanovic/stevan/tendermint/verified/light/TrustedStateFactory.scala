package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStore
import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStoreFactory.LightStoreConfiguration
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStateFactory.TrustedStateConfiguration
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifiedStates.TrustedState
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock

trait TrustedStateFactory {
  def trustedState(configuration: TrustedStateConfiguration): TrustedState
}

object TrustedStateFactory {

  trait TrustedStateConfiguration {
    def votingPowerVerifier: VotingPowerVerifier
    def trustedLightBlock: LightBlock
  }

  case class SimpleTrustedStateConfiguration(trustedLightBlock: LightBlock, votingPowerVerifier: VotingPowerVerifier)
      extends TrustedStateConfiguration

  /**
   * Configuration for `LightStoreBackedTrustedState`
   * @param trustedLightBlock initial trusted light block
   * @param votingPowerVerifier to be used with trusted states
   * @param lightStoreConfiguration configuration for the backing `LightStore` or a specific instance to be used
   */
  case class LightStoreBackedTrustedStateConfiguration(
    trustedLightBlock: LightBlock,
    votingPowerVerifier: VotingPowerVerifier,
    lightStoreConfiguration: Either[LightStoreConfiguration, LightStore])
      extends TrustedStateConfiguration

}
