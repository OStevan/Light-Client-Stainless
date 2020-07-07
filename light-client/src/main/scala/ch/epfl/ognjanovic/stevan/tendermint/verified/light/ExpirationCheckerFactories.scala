package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Duration

object ExpirationCheckerFactories {

  trait ExpirationCheckerConfiguration

  case class TimeBasedExpirationCheckerConfig(timeSupplier: () ⇒ Instant, duration: Duration)
      extends ExpirationCheckerConfiguration

  trait ExpirationCheckerFactory {
    def constructChecker(expirationCheckerConfiguration: ExpirationCheckerConfiguration): ExpirationChecker
  }

  object DefaultExpirationCheckerFactory extends ExpirationCheckerFactory {

    override def constructChecker(checkerConfiguration: ExpirationCheckerConfiguration): ExpirationChecker =
      checkerConfiguration match {
        case TimeBasedExpirationCheckerConfig(timeSupplier, duration) ⇒
          new TimeBasedExpirationChecker(timeSupplier, duration)
        case _ ⇒ ???
      }

  }

}
