package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import java.nio.{ByteBuffer => ByteArray}
import java.time.Instant

import stainless.annotation.{extern, pure}

case class Header(
  @extern @pure version: Consensus,
  @extern @pure chainId: String,
  height: Height,
  @extern @pure time: Instant,
  @extern @pure lastBlockId: BlockId,
  @extern @pure lastCommit: ByteArray,
  @extern @pure data: ByteArray,
  @extern @pure validators: ByteArray,
  @extern @pure nextValidators: ByteArray,
  @extern @pure consensus: ByteArray,
  @extern @pure app: ByteArray,
  @extern @pure lastResults: ByteArray,
  @extern @pure evidence: ByteArray,
  proposer: Address)
