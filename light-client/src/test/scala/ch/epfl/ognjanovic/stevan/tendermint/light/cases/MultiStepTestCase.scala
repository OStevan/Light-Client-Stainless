package ch.epfl.ognjanovic.stevan.tendermint.light.cases

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.light.{InMemoryProvider, VerifierTests}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import io.circe._
import io.circe.generic.semiauto._

case class MultiStepTestCase(
  trust_options: TrustOptions,
  primary: LightBlockProvider,
  height_to_verify: Long,
  now: Instant)

object MultiStepTestCase {

  implicit private val lightBlockProviderDecoder: Decoder[LightBlockProvider] =
    InMemoryProvider.decoder(VerifierTests.lightBlockDecoder)

  implicit val decoder: Decoder[MultiStepTestCase] = deriveDecoder
}
