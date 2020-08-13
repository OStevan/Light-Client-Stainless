package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import java.time.Instant
import java.time.temporal.ChronoUnit

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock

import scala.concurrent.duration.Duration

class InstantTimeValidator(timeProvider: () => Instant, trustingPeriod: Duration, clock_drift: Duration)
    extends TimeValidator {

  override def isExpired(lightBlock: LightBlock): Boolean =
    ChronoUnit.NANOS.between(
      Instant.ofEpochSecond(lightBlock.header.time.seconds.toLong, lightBlock.header.time.nanos.toLong),
      timeProvider()) >= trustingPeriod.toNanos

  override def fromFuture(lightBlock: LightBlock): Boolean =
    timeProvider()
      .plusNanos(clock_drift.toNanos)
      .compareTo(Instant.ofEpochSecond(lightBlock.header.time.seconds.toLong, lightBlock.header.time.nanos.toLong)) < 0

}
