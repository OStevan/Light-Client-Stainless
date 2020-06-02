package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import stainless.annotation.pure

object NextHeightCalculators {

  sealed abstract class NextHeightCalculator {
    @pure
    def nextHeight(bottom: Height, top: Height): Height = {
      require(bottom + 1 < top)
      ??? : Height
    }.ensuring(res => bottom < res && res < top)
  }

  case object BisectionHeightCalculator extends NextHeightCalculator {
    @pure
    override def nextHeight(bottom: Height, top: Height): Height = {
      require(bottom + 1 < top)
      (bottom + top) / 2
    }.ensuring(res => res > bottom && res < top)
  }

  case object SequentialHeightCalculator extends NextHeightCalculator {
    @pure
    override def nextHeight(bottom: Height, top: Height): Height = {
      require(bottom + 1 < top)
      bottom + 1
    }.ensuring(res => res > bottom && res < top)
  }

}
