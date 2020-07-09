package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.UntrustedTraces.{InMemoryUntrustedTrace, UntrustedTrace}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height

object UntrustedTraceFactories {

  trait UntrustedTraceFactory {
    def emptyWithTarget(target: Height): UntrustedTrace
  }

  sealed class InMemoryUntrustedTraceFactory extends UntrustedTraceFactory {

    override def emptyWithTarget(target: Height): UntrustedTrace =
      InMemoryUntrustedTrace(target, stainless.collection.List.empty)

  }

}
