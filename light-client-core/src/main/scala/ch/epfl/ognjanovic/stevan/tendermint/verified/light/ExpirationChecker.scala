package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.annotation.{extern, pure}

abstract class ExpirationChecker {

  @pure @extern
  def isExpired(lightBlock: LightBlock): Boolean

}
