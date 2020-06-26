package ch.epfl.ognjanovic.stevan.tendermint.light

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.ExpirationChecker
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Duration

object ExpirationCheckerFactories {

  trait ExpirationCheckerFactory {
    def constructChecker(duration: Duration): ExpirationChecker
  }

  class FixedTimeExpirationCheckerFactory(private val time: Instant) extends ExpirationCheckerFactory {

    override def constructChecker(duration: Duration): ExpirationChecker = {
      new TimeBasedExpirationChecker(() ⇒ time, duration)
    }

  }

  object CurrentTimeExpirationCheckerFactory extends ExpirationCheckerFactory {

    override def constructChecker(duration: Duration): ExpirationChecker = {
      new TimeBasedExpirationChecker(() ⇒ Instant.now(), duration)
    }

  }

}
