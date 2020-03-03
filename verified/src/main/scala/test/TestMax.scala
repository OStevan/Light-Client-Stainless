package test

import stainless.lang._
import stainless.collection._
import stainless.annotation._

object TestMax {
  def max(x: Int, y: Int): Int = {
    require(x >= 0 && y >= 0)
    val d = x - y
    if (d > 0) x
    else y
  } ensuring (res => x <= res && y <= res && (res == x || res == y))

  def min(x: Int, y: Int): Int = {
    require(x >= 0 && y >= 0)
    val diff = x - y
    if (diff > 0) y
    else x
  } ensuring (res => x >= res && y >= res && (res == x || res == y))
}