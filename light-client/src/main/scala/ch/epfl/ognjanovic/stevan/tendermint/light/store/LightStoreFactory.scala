package ch.epfl.ognjanovic.stevan.tendermint.light.store

trait LightStoreFactory {
  def lightStore(): LightStore
}
