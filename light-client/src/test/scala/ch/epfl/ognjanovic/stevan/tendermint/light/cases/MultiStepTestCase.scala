package ch.epfl.ognjanovic.stevan.tendermint.light.cases

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.light.cases.MultiStepTestCase.{PeerInput, WitnessInput}
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.circe.instantDecoder
import io.circe._
import io.circe.generic.semiauto._
import TrustOptions.decoder
import InputFormat.inputFormatDecoder
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.CirceDecoders.{
  conformanceTestValidatorSetDecoder,
  signedHeaderDecoder
}

case class MultiStepTestCase(
  trust_options: TrustOptions,
  primary: PeerInput,
  witnesses: Option[Array[WitnessInput]],
  height_to_verify: Long,
  now: Instant)

object MultiStepTestCase {

  case class PeerInput(chain_id: String, lite_blocks: Array[InputFormat])

  case class WitnessInput(`type`: String, value: PeerInput)

  implicit private val peerInputDecoder: Decoder[PeerInput] = deriveDecoder

  implicit private val witnessInputDecoder: Decoder[WitnessInput] = deriveDecoder

  implicit val decoder: Decoder[MultiStepTestCase] = deriveDecoder
}
