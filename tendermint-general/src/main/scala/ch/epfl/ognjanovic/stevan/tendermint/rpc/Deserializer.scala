package ch.epfl.ognjanovic.stevan.tendermint.rpc

import stainless.annotation.ignore

@ignore
trait Deserializer[T] {
  def apply(jsonString: String): T
}
