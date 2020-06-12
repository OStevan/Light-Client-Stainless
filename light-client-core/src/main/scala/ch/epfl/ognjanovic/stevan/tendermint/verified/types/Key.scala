package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import stainless.annotation.{extern, pure}

import scala.collection.Seq

case class Key(@extern @pure tpe: String, @extern @pure value: Seq[Byte])
