package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock

abstract class TimeValidator {

  def isExpired(lightBlock: LightBlock): Boolean

  def fromFuture(lightBlock: LightBlock): Boolean
}
