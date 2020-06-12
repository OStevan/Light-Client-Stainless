package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import stainless.annotation.{extern, pure}

import scala.collection.Seq

case class Address(@extern @pure address: Seq[Byte])
