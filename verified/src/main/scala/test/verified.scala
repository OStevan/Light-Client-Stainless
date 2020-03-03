package test

import stainless.lang._
import stainless.collection._
import stainless.annotation._

object verified {

  def test = {
    1 + 1 == 2
  }.holds

def factorial(n: BigInt): BigInt = {
  require(n >= 0)
  if(n == 0) {
    BigInt(1)
  } else {
    n * factorial(n - 1)
  }
 } ensuring((res: BigInt) => res >= 0)
}
