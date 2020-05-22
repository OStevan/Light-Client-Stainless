package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import java.nio.ByteBuffer

import stainless.annotation.{extern, pure}

case class Key(@extern @pure tpe: String, @extern @pure value: ByteBuffer)
