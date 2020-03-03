package test

import stainless.lang._
import stainless.collection._
import stainless.annotation._

object TestMax {
  def max(x: BigInt, y: BigInt): BigInt = {
    val d = x - y
    if (d > 0) x
    else y
  } ensuring (res => x <= res && y <= res && (res == x || res == y))

  def min(x: BigInt, y: BigInt): BigInt = {
    val diff = x - y
    if (diff > 0) y
    else x
  } ensuring (res => x >= res && y >= res && (res == x || res == y))

  def maxLemma(x: BigInt, y: BigInt, z: BigInt) = {
    max(x,x) == x &&
    max(x,y) == max(y,x) &&
    max(x,max(y,z)) == max(max(x,y), z) &&
    max(x,y) + z == max(x + z, y + z)
  }.holds
}