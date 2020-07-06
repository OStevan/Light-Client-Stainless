package ch.epfl.ognjanovic.stevan.tendermint.light.store

import ch.epfl.ognjanovic.stevan.tendermint.light.store.InMemoryLightStore.heightOrdering
import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStoreFactory._
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}

import scala.collection.mutable

sealed class DefaultLightStoreFactory extends LightStoreFactory {

  override def lightStore(config: LightStoreConfiguration): LightStore = config match {
    case InMemoryLightStoreConfiguration ⇒
      new InMemoryLightStore(mutable.SortedMap.empty[Height, LightBlock], mutable.SortedMap.empty, mutable.Map.empty)
    case _ ⇒ ???
  }

}
