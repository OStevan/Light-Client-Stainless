package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.FetchedStacks.{FetchedStack, InMemoryFetchedStack}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height

object FetchedStackFactories {

  trait FetchedStackFactory {
    def emptyWithTarget(target: Height): FetchedStack
  }

  sealed class InMemoryFetchedStackFactory extends FetchedStackFactory {

    override def emptyWithTarget(target: Height): FetchedStack =
      InMemoryFetchedStack(target, stainless.collection.List.empty)

  }

}
