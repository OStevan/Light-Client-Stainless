package ch.epfl.ognjanovic.stevan.tendermint.rpc

import com.dimafeng.testcontainers.GenericContainer.Def
import com.dimafeng.testcontainers.scalatest.TestContainerForAll
import org.scalatest.flatspec.AnyFlatSpec
import stainless.annotation.ignore

@ignore
class RpcTests extends AnyFlatSpec with TestContainerForAll {

  override val containerDef: Def[TendermintSingleNodeContainer] = TendermintSingleNodeContainer.Def()

  // To use containers in tests you need to use `withContainers` function
  it should "test" in withContainers { myContainer =>
    // Inside your test body you can do with your container whatever you want to
    println(myContainer.rpcPort)
    println(myContainer.url)
  }
}
