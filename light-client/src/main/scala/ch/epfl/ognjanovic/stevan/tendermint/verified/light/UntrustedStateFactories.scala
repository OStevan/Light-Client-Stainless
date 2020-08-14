package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.FetchedStacks.{InMemoryUntrustedState, UntrustedState}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height

object UntrustedStateFactories {

  trait UntrustedStateFactory {
    def emptyWithTarget(target: Height): UntrustedState
  }

  sealed class InMemoryUntrustedStateFactory extends UntrustedStateFactory {

    override def emptyWithTarget(target: Height): UntrustedState =
      InMemoryUntrustedState(target, stainless.collection.List.empty)

  }

}
