package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import java.time.Instant

import scala.concurrent.duration.Duration

object TimeValidatorFactories {

  trait TimeValidatorConfig

  case class InstantTimeValidatorConfig(timeSupplier: () ⇒ Instant, duration: Duration, clock_drift: Duration)
      extends TimeValidatorConfig

  trait TimeValidatorFactory {
    def constructChecker(timeValidatorConfig: TimeValidatorConfig): TimeValidator
  }

  object DefaultTimeValidatorFactory extends TimeValidatorFactory {

    override def constructChecker(timeValidatorConfig: TimeValidatorConfig): TimeValidator =
      timeValidatorConfig match {
        case InstantTimeValidatorConfig(timeSupplier, duration, clock_drift) ⇒
          new InstantTimeValidator(timeSupplier, duration, clock_drift)
        case _ ⇒ ???
      }

  }

}
