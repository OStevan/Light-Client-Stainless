package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Nodes.Node
import stainless.annotation.pure
import stainless.lang._

object SignedHeaders {

    abstract class SignedHeader {
        @pure
        def header: BlockHeader

        @pure
        def commit: Set[Node]

        def isExpired(): Boolean
    }

    case class DefaultSignedHeader(header: BlockHeader, commit: Set[Node]) extends SignedHeader {
        def isExpired(): Boolean = false
    }

}