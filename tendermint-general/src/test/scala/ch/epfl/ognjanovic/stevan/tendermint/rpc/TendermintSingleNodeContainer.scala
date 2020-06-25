package ch.epfl.ognjanovic.stevan.tendermint.rpc

import com.dimafeng.testcontainers.GenericContainer
import stainless.annotation.ignore

@ignore
class TendermintSingleNodeContainer(underlying: GenericContainer) extends GenericContainer(underlying) {

  def rpcPort: Int = underlying.mappedPort(TendermintSingleNodeContainer.INTERNAL_RPC_PORT)

  def url: String = underlying.container.getContainerIpAddress
}

@ignore
object TendermintSingleNodeContainer {
  private val INTERNAL_RPC_PORT = 26657

  // In the container definition you need to describe, how your container will be constructed:
  case class Def() extends GenericContainer.Def[TendermintSingleNodeContainer](
    new TendermintSingleNodeContainer(
      new GenericContainer(
        "tendermint/tendermint:v0.33.5",
        exposedPorts = Seq(INTERNAL_RPC_PORT)
      )
    )
  )

}
