package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.NextHeightCalculators.NextHeightCalculator
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TimeValidatorFactories.TimeValidatorConfig
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerifierFactories.VerifierFactory
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier

object MultiStepVerifierFactories {

  trait MultiStepVerifierFactory {

    def constructVerifier(
      lightBlockProvider: LightBlockProvider,
      votingPowerVerifier: VotingPowerVerifier,
      timeValidatorConfig: TimeValidatorConfig): MultiStepVerifier

  }

  class DefaultMultiStepVerifierFactory(
    private val verifierFactory: VerifierFactory,
    private val nextHeightCalculator: NextHeightCalculator)
      extends MultiStepVerifierFactory {

    override def constructVerifier(
      lightBlockProvider: LightBlockProvider,
      votingPowerVerifier: VotingPowerVerifier,
      timeValidatorConfig: TimeValidatorConfig): MultiStepVerifier = {
      val singleStepVerifier = verifierFactory.constructInstance(votingPowerVerifier, timeValidatorConfig)

      MultiStepVerifier(lightBlockProvider, singleStepVerifier, nextHeightCalculator)
    }

  }

}
