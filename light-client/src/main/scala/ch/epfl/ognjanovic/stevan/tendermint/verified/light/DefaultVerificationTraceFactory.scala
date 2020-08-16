package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStoreFactory
import ch.epfl.ognjanovic.stevan.tendermint.light.LightBlockStatuses.Trusted
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VerificationTraces.{
  StartingVerificationTrace,
  VerificationTrace
}

class DefaultVerificationTraceFactory(private val lightStoreFactory: LightStoreFactory) extends VerificationTraceFactory {

  override def verificationTrace(
    configuration: VerificationTraceFactory.VerifiedStateConfiguration): VerificationTrace =
    configuration match {
      case VerificationTraceFactory.LightStoreBackedVerifiedStateConfiguration(
            trustedLightBlock,
            votingPowerVerifier,
            lightStoreConfiguration) ⇒
        val store = lightStoreConfiguration match {
          case Left(value) ⇒ lightStoreFactory.lightStore(value)
          case Right(value) ⇒ value
        }

        store.update(trustedLightBlock, Trusted)
        new LightStoreBackedVerificationTrace(store, votingPowerVerifier)

      case VerificationTraceFactory.SimpleVerifiedStateConfiguration(trustedLightBlock, votingPowerVerifier) ⇒
        StartingVerificationTrace(trustedLightBlock, votingPowerVerifier)

      case _ ⇒ ???
    }

}
