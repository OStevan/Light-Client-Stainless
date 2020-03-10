package ch.epfl.ognjanovic.stevan.library

import stainless.lang._
import ch.epfl.ognjanovic.stevan.library.Nodes.Node

case class SignedHeader(header: BlockHeader, commit: Set[Node])