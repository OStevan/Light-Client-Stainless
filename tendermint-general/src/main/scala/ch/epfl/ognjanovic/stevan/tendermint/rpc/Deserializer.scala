package ch.epfl.ognjanovic.stevan.tendermint.rpc

@ignore
trait Deserializer[T] {
  def apply(jsonString: String): T
}
