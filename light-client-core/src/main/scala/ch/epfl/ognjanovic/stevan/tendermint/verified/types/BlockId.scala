package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import stainless.annotation.{extern, pure}

import scala.collection.Seq

case class BlockId(@extern @pure bytes: Seq[Byte], parts: PartSetHeader)
