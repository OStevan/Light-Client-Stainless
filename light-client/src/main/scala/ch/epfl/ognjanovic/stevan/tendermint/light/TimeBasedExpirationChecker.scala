package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant
import java.time.temporal.ChronoUnit

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.ExpirationChecker
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock

class TimeBasedExpirationChecker(timeProvider: () => Instant, trustingPeriod: BigInt) extends ExpirationChecker {
  override def isExpired(lightBlock: LightBlock): Boolean =
    ChronoUnit.NANOS.between(
      Instant.ofEpochSecond(
        lightBlock.header.time.seconds.toLong, lightBlock.header.time.nanos.toLong), timeProvider()) >= trustingPeriod
}
