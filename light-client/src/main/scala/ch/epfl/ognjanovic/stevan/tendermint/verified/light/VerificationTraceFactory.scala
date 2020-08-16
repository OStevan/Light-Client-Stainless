package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStore
import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStoreFactory.LightStoreConfiguration
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationTraceFactory.VerifiedStateConfiguration
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationTraces.VerificationTrace
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock

trait VerificationTraceFactory {
  def verificationTrace(configuration: VerifiedStateConfiguration): VerificationTrace
}

object VerificationTraceFactory {

  trait VerifiedStateConfiguration {
    def votingPowerVerifier: VotingPowerVerifier
    def trustedLightBlock: LightBlock
  }

  case class SimpleVerifiedStateConfiguration(trustedLightBlock: LightBlock, votingPowerVerifier: VotingPowerVerifier)
      extends VerifiedStateConfiguration

  /**
   * Configuration for `LightStoreBackedVerifiedState`
   * @param trustedLightBlock initial trusted light block
   * @param votingPowerVerifier to be used with trusted states
   * @param lightStoreConfiguration configuration for the backing `LightStore` or a specific instance to be used
   */
  case class LightStoreBackedVerifiedStateConfiguration(
    trustedLightBlock: LightBlock,
    votingPowerVerifier: VotingPowerVerifier,
    lightStoreConfiguration: Either[LightStoreConfiguration, LightStore])
      extends VerifiedStateConfiguration

}
