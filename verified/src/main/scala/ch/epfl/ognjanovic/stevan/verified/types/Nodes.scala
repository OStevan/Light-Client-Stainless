package ch.epfl.ognjanovic.stevan.verified.types

/**
 * Defines interface modeling nodes which are used by blockchain.
 */
object Nodes {

  sealed abstract class Node

  case class SimpleNode(id: Int) extends Node {
    require(id > 0)
  }

}
