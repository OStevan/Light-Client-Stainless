package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import stainless.annotation.{extern, pure}

import scala.collection.Seq

case class PartSetHeader(total: Int, @extern @pure hash: Seq[Byte])
