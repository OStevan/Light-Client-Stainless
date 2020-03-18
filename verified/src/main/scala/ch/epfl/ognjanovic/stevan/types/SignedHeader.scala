package ch.epfl.ognjanovic.stevan.types

import ch.epfl.ognjanovic.stevan.types.Nodes.Node

case class SignedHeader(header: BlockHeader, commit: Set[Node])