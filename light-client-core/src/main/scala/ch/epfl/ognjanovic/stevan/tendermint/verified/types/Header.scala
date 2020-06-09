package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import java.time.Instant

import stainless.annotation.{extern, pure}

case class Header(
  @extern @pure version: Consensus,
  @extern @pure chainId: String,
  height: Height,
  @extern @pure time: Instant,
  @extern @pure lastBlockId: BlockId,
  @extern @pure lastCommit: Seq[Byte],
  @extern @pure data: Seq[Byte],
  @extern @pure validators: Seq[Byte],
  @extern @pure nextValidators: Seq[Byte],
  @extern @pure consensus: Seq[Byte],
  @extern @pure app: Seq[Byte],
  @extern @pure lastResults: Seq[Byte],
  @extern @pure evidence: Seq[Byte],
  proposer: Address)
