package ch.epfl.ognjanovic.stevan.library

/**
  * Defines interface modeling nodes which are used by blockchain.
  */
object Nodes {
    sealed abstract class Node
    case class SimpleNode() extends Node
}
