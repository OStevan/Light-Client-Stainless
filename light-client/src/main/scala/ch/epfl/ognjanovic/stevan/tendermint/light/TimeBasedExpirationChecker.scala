package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant
import java.time.temporal.ChronoUnit

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.ExpirationChecker
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock

class TimeBasedExpirationChecker(timeProvider: () => Instant, trustingPeriod: BigInt) extends ExpirationChecker {
  override def isExpired(lightBlock: LightBlock): Boolean =
    ChronoUnit.NANOS.between(lightBlock.header.time, timeProvider()) >= trustingPeriod
}
