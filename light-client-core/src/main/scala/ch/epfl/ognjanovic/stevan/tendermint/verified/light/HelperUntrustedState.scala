package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock
import stainless.annotation.pure
import stainless.collection._
import stainless.lang.StaticChecks.Ensuring

case class HelperUntrustedState(pending: List[LightBlock]) {
  require(HelperUntrustedState.pendingInvariant(pending))

  @pure
  def addSignedHeader(lightBlock: LightBlock): HelperUntrustedState = {
    require(pending.isEmpty || (lightBlock.header.height < pending.head.header.height))
    HelperUntrustedState(lightBlock :: pending)
  }.ensuring(res =>
    (pending.isEmpty || pending.reverse.head == res.pending.reverse.head) &&
      res.pending.head.header.height == lightBlock.header.height &&
      res.pending.nonEmpty
  )

  def size: BigInt = pending.size
}

object HelperUntrustedState {
  @pure
  def empty: HelperUntrustedState = HelperUntrustedState(Nil[LightBlock]())

  @pure
  def apply(lightBlock: LightBlock): HelperUntrustedState = HelperUntrustedState(Cons(lightBlock, Nil()))

  @scala.annotation.tailrec
  def pendingInvariant(pending: List[LightBlock]): Boolean = {
    pending match {
      case Cons(first, tail) if tail.isInstanceOf[Cons[LightBlock]] =>
        first.header.height < tail.head.header.height && pendingInvariant(tail)
      case _ => true
    }
  }
}
