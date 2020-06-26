package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider

trait LightBlockProviderFactory {
  def constructProvider(secure: Boolean, url: String, port: Option[Int]): LightBlockProvider
}
