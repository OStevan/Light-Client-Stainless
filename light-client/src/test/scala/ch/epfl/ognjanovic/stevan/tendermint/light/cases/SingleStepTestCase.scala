package ch.epfl.ognjanovic.stevan.tendermint.light.cases

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.light.{InMemoryProvider, VerifierTests}
import ch.epfl.ognjanovic.stevan.tendermint.light.cases.SingleStepTestCase.TrustedInitialState
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.circe.instantDecoder
import ch.epfl.ognjanovic.stevan.tendermint.rpc.types.SignedHeader
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.ValidatorSet
import io.circe.generic.semiauto.deriveDecoder
import io.circe.Decoder

case class SingleStepTestCase(initial: TrustedInitialState, input: LightBlockProvider)

object SingleStepTestCase {

  case class TrustedInitialState(
    signed_header: SignedHeader,
    next_validator_set: ValidatorSet,
    now: Instant,
    trusting_period: Long)

  implicit private val lightBlockProviderDecoder: Decoder[LightBlockProvider] =
    InMemoryProvider.defaultChainDecoder(VerifierTests.lightBlockDecoder)

  implicit private val trustedInitialStateDecoder: Decoder[TrustedInitialState] = deriveDecoder

  implicit val decoder: Decoder[SingleStepTestCase] = deriveDecoder

}
