package ch.epfl.ognjanovic.stevan.tendermint

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.light.cases.{MultiStepTestCase, SingleStepTestCase, TrustOptions}
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.circe.instantDecoder
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.TrustedStates.{SimpleTrustedState, TrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.VotingPowerVerifiers.VotingPowerVerifier
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Duration, Height, LightBlock}
import io.circe.Decoder

package object light {

  implicit val singleStepTestCaseDecoder
    : Decoder[(TrustedState, VotingPowerVerifier, Duration, Instant, LightBlockProvider)] =
    cursor =>
      for {
        testCase ← cursor.as[SingleStepTestCase](SingleStepTestCase.decoder)
      } yield {
        val trustVerifier = VotingPowerVerifiers.defaultTrustVerifier

        (
          SimpleTrustedState(
            LightBlock(
              testCase.initial.signed_header.header,
              testCase.initial.signed_header.commit,
              testCase.initial.next_validator_set,
              testCase.initial.next_validator_set,
              VerifierTests.defaultProvider
            ),
            trustVerifier
          ),
          trustVerifier,
          Duration(0, testCase.initial.trusting_period),
          testCase.initial.now,
          testCase.input)
      }

  implicit val multiStepTestCaseDecoder: Decoder[(TrustOptions, LightBlockProvider, Height, Instant)] = cursor =>
    for {
      testCase <- cursor.as[MultiStepTestCase](MultiStepTestCase.decoder)
    } yield {
      (testCase.trust_options, testCase.primary, Height(testCase.height_to_verify), testCase.now)
    }

}
