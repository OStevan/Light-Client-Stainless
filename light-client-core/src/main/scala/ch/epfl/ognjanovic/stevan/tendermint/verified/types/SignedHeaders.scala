package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import stainless.annotation.pure

object SignedHeaders {

    abstract class SignedHeader {
        @pure
        def header: BlockHeader

        @pure
        def commit: Commit

        def isExpired(): Boolean
    }

    case class DefaultSignedHeader(header: BlockHeader, commit: Commit) extends SignedHeader {
        def isExpired(): Boolean = false
    }

}