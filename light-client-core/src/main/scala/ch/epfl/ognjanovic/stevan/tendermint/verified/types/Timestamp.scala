package ch.epfl.ognjanovic.stevan.tendermint.verified.types
import stainless.annotation._

/**
 * Timestamp visible for Stainless, treated as UTC epoch Instant
 */
case class Timestamp(seconds: BigInt, nanos: BigInt) {
  def <(other: Timestamp): Boolean = {
    if (seconds < other.seconds)
      true
    else if (seconds == other.seconds && nanos < other.seconds)
      true
    else
      false
  }

  /**
   * Time can only go forward.
   */
  @pure
  def addTime(seconds: BigInt, nanos: BigInt): Timestamp = {
    require(seconds > 0 && nanos > 0)
    Timestamp(this.seconds + seconds, this.nanos + nanos)
  }
}
