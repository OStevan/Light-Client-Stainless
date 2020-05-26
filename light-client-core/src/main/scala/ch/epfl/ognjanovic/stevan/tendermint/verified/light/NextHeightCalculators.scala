package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height

object NextHeightCalculators {

  sealed abstract class NextHeightCalculator {
    def nextHeight(bottom: Height, top: Height): Height = {
      require(bottom + 1 < top)
      ??? : Height
    }.ensuring(res => res > bottom && res < top)
  }

  case object BisectionHeightCalculator extends NextHeightCalculator {
    override def nextHeight(bottom: Height, top: Height): Height = {
      require(bottom + 1 < top)
      (bottom + top) / 2
    }.ensuring(res => res > bottom && res < top)
  }

}
