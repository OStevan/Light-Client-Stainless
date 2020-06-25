package ch.epfl.ognjanovic.stevan.tendermint.rpc

import com.dimafeng.testcontainers.GenericContainer.Def
import com.dimafeng.testcontainers.scalatest.TestContainerForAll
import org.scalatest.flatspec.AnyFlatSpec
import stainless.annotation.ignore
import sttp.client.HttpURLConnectionBackend

@ignore
class RpcTests extends AnyFlatSpec with TestContainerForAll {

  override val containerDef: Def[TendermintSingleNodeContainer] = TendermintSingleNodeContainer.Def()

  override def afterContainersStart(containers: TendermintSingleNodeContainer): Unit = {
    super.afterContainersStart(containers)
    Thread.sleep(500)
  }

  // To use containers in tests you need to use `withContainers` function
  it should "test" in withContainers { myContainer =>
    // Inside your test body you can do with your container whatever you want to
    val client = new TendermintFullNodeClient(
      false,
      myContainer.url,
      Some(myContainer.rpcPort),
      HttpURLConnectionBackend())
    for {
      _ <- 1 to 10
    } {
      println(client.commit(Option.empty))
      println(client.validatorSet(Option.empty))
    }
  }
}
