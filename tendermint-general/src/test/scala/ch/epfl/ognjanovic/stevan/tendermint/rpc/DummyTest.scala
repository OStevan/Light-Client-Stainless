package ch.epfl.ognjanovic.stevan.tendermint.rpc

import com.dimafeng.testcontainers.MySQLContainer
import com.dimafeng.testcontainers.scalatest.TestContainerForAll
import org.scalatest.flatspec.AnyFlatSpec
import stainless.annotation.ignore

@ignore
class DummyTest extends AnyFlatSpec with TestContainerForAll {

  // You need to override `containerDef` with needed container definition
  override val containerDef = MySQLContainer.Def()

  // To use containers in tests you need to use `withContainers` function
  it should "test" in withContainers { mysqlContainer =>
    // Inside your test body you can do with your container whatever you want to
    println(mysqlContainer.testQueryString)
  }
}
