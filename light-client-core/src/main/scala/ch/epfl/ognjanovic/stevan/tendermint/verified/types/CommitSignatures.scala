package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import java.time.Instant

import stainless.annotation.{extern, pure}

object CommitSignatures {

  sealed abstract class CommitSignature

  case object BlockIDFlagAbsent extends CommitSignature

  case class BlockIDFlagCommit(
    validator: Address,
    @extern @pure timestamp: Instant,
    @extern @pure signature: Seq[Byte]) extends CommitSignature

  case class BlockIdFlagNil(
    validator: Address,
    @extern @pure timestamp: Instant,
    @extern @pure signature: Seq[Byte]) extends CommitSignature {
  }

}
