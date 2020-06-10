package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import stainless.annotation.{extern, pure}

case class Address(@extern @pure address: Seq[Byte])
