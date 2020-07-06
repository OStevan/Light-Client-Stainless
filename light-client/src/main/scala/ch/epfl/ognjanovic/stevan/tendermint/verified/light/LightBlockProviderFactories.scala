package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.rpc.{RpcRequester, TendermintFullNodeClient}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.PeerId
import sttp.client.HttpURLConnectionBackend

object LightBlockProviderFactories {

  trait LightBlockProviderFactory {
    def constructProvider(secure: Boolean, url: String, port: Option[Int]): LightBlockProvider
  }

  class DefaultLightBlockProviderFactory extends LightBlockProviderFactory {

    override def constructProvider(secure: Boolean, url: String, port: Option[Int]): LightBlockProvider = {
      val client =
        new TendermintFullNodeClient(secure, url, port, HttpURLConnectionBackend())

      val nodeStatus = client.status()

      new DefaultProvider(
        nodeStatus.node_info.network,
        new RpcRequester(PeerId(nodeStatus.validator_info.pub_key), client))
    }

  }

}
