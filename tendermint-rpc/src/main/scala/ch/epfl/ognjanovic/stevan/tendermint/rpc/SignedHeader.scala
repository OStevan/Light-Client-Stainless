package ch.epfl.ognjanovic.stevan.tendermint.rpc

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Commit, Header}

case class SignedHeader(header: Header, commit: Commit)
