package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Nodes.Node
import stainless.lang._

object SignedHeader {

    abstract class SignedHeader {
        def header: BlockHeader

        def commit: Set[Node]

        def isExpired(): Boolean
    }

    case class DefaultSignedHeader(header: BlockHeader, commit: Set[Node]) extends SignedHeader {
        def isExpired(): Boolean = false
    }

}