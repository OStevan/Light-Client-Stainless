package ch.epfl.ognjanovic.stevan.tendermint.verified.types

import stainless.annotation._

/**
 * Timestamp visible for Stainless, treated as UTC epoch Instant
 */
case class Timestamp(seconds: BigInt, nanos: BigInt) {

  def <=(other: Timestamp): Boolean = {
    if (seconds < other.seconds)
      true
    else if (seconds == other.seconds && nanos <= other.nanos)
      true
    else
      false
  }

  def >(other: Timestamp): Boolean = {
    if (seconds > other.seconds)
      true
    else if (seconds == other.seconds && nanos > other.nanos)
      true
    else
      false
  }

  /**
   * Time can only go forward.
   */
  @pure
  def addTime(duration: Duration): Timestamp = {
    require(duration.seconds >= 0 && duration.nanos >= 0)
    Timestamp(this.seconds + duration.seconds, this.nanos + duration.nanos)
  }

}
