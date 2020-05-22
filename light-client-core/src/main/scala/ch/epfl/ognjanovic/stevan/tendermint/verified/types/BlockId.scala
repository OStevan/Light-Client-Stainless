package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import java.nio.ByteBuffer

import stainless.annotation.{extern, pure}

case class BlockId(@extern @pure bytes: ByteBuffer, parts: PartSetHeader)
