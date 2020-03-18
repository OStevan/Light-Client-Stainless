package ch.epfl.ognjanovic.stevan.types

import ch.epfl.ognjanovic.stevan.types.Nodes.Node
import stainless.lang._

case class SignedHeader(header: BlockHeader, commit: Set[Node])