package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import java.time.Instant

import scala.concurrent.duration.Duration

object TimeValidatorFactories {

  trait ExpirationCheckerConfiguration

  case class TimeBasedExpirationCheckerConfig(timeSupplier: () ⇒ Instant, duration: Duration, clock_drift: Duration)
      extends ExpirationCheckerConfiguration

  trait ExpirationCheckerFactory {
    def constructChecker(expirationCheckerConfiguration: ExpirationCheckerConfiguration): TimeValidator
  }

  object DefaultExpirationCheckerFactory extends ExpirationCheckerFactory {

    override def constructChecker(checkerConfiguration: ExpirationCheckerConfiguration): TimeValidator =
      checkerConfiguration match {
        case TimeBasedExpirationCheckerConfig(timeSupplier, duration, clock_drift) ⇒
          new InstantTimeValidator(timeSupplier, duration, clock_drift)
        case _ ⇒ ???
      }

  }

}
