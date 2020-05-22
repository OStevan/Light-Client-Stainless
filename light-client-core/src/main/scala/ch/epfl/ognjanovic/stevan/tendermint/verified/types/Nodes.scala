package ch.epfl.ognjanovic.stevan.tendermint.verified.types

/**
 * Defines interface modeling nodes which are used by blockchain.
 */
object Nodes {

  sealed abstract class PeerId

  case class SimplePeerId(publicKey: Key) extends PeerId

}
