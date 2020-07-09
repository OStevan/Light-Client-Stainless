package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.rpc.{RpcRequester, TendermintFullNodeClient}
import ch.epfl.ognjanovic.stevan.tendermint.verified.light.LightBlockProviders.LightBlockProvider
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock, PeerId}
import com.github.blemale.scaffeine.{Cache, Scaffeine}
import sttp.client.HttpURLConnectionBackend
import scala.concurrent.duration._

object LightBlockProviderFactories {

  trait LightBlockProviderFactory {
    def constructProvider(secure: Boolean, url: String, port: Option[Int]): LightBlockProvider
  }

  sealed class DefaultLightBlockProviderFactory extends LightBlockProviderFactory {

    override def constructProvider(secure: Boolean, url: String, port: Option[Int]): LightBlockProvider = {
      val client =
        new TendermintFullNodeClient(secure, url, port, HttpURLConnectionBackend())

      val nodeStatus = client.status()

      new DefaultProvider(
        nodeStatus.node_info.network,
        new RpcRequester(PeerId(nodeStatus.validator_info.pub_key), client))
    }

  }

  sealed class CachingLightBlockProviderFactory(
    private val expirationDuration: Duration,
    private val maxSize: Int,
    private val lightBlockProviderFactory: LightBlockProviderFactory)
      extends LightBlockProviderFactory {

    private val sharedCache: Cache[(PeerId, Height), LightBlock] = Scaffeine()
      .recordStats()
      .expireAfterWrite(expirationDuration)
      .maximumSize(maxSize)
      .build[(PeerId, Height), LightBlock]()

    override def constructProvider(secure: Boolean, url: String, port: Option[Int]): LightBlockProvider = {
      new CachingLightBlockProvider(lightBlockProviderFactory.constructProvider(secure, url, port), sharedCache)
    }

  }

}
