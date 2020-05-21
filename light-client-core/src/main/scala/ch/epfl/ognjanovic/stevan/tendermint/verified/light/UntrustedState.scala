package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.SignedHeaders.SignedHeader
import stainless.annotation.pure
import stainless.collection._
import stainless.lang.StaticChecks.Ensuring

case class UntrustedState(pending: List[SignedHeader]) {
  require(UntrustedState.pendingInvariant(pending))

  @pure
  def addSignedHeader(signedHeader: SignedHeader): UntrustedState = {
    require(pending.isEmpty || (signedHeader.header.height < pending.head.header.height))
    UntrustedState(signedHeader :: pending)
  }.ensuring(res =>
    (pending.isEmpty || pending.reverse.head == res.pending.reverse.head) &&
      res.pending.head.header.height == signedHeader.header.height &&
      res.pending.nonEmpty
  )

  def size: BigInt = pending.size
}

object UntrustedState {
  @pure
  def empty: UntrustedState = UntrustedState(Nil[SignedHeader]())

  @pure
  def apply(signedHeader: SignedHeader): UntrustedState = UntrustedState(Cons(signedHeader, Nil()))

  @scala.annotation.tailrec
  def pendingInvariant(pending: List[SignedHeader]): Boolean = {
    pending match {
      case Cons(first, tail) if tail.isInstanceOf[Cons[SignedHeader]] =>
        first.header.height < tail.head.header.height && pendingInvariant(tail)
      case _ => true
    }
  }
}
