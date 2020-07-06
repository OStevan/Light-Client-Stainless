package ch.epfl.ognjanovic.stevan.tendermint.light.store

import ch.epfl.ognjanovic.stevan.tendermint.light.store.LightStoreFactory.LightStoreConfiguration

trait LightStoreFactory {
  def lightStore(config: LightStoreConfiguration): LightStore
}

object LightStoreFactory {
  trait LightStoreConfiguration

  case object InMemoryLightStoreConfiguration extends LightStoreConfiguration
}
