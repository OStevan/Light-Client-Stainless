package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import java.nio.ByteBuffer
import java.time.Instant

import stainless.annotation.{extern, pure}

case class Signature(
  blockIdFlag: Byte,
  validatorAddress: Address,
  @extern @pure timestamp: Instant,
  @extern @pure signature: ByteBuffer)
