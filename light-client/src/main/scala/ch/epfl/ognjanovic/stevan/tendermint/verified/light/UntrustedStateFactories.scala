package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.FetchedStacks.{FetchedStack, InMemoryFetchedStack}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height

object UntrustedStateFactories {

  trait UntrustedStateFactory {
    def emptyWithTarget(target: Height): FetchedStack
  }

  sealed class InMemoryUntrustedStateFactory extends UntrustedStateFactory {

    override def emptyWithTarget(target: Height): FetchedStack =
      InMemoryFetchedStack(target, stainless.collection.List.empty)

  }

}
