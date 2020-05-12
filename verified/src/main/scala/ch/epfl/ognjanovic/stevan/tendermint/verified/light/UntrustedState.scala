package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.SignedHeader.SignedHeader
import stainless.collection._

case class UntrustedState(pending: List[SignedHeader]) {
  require(UntrustedState.pendingInvariant(pending))

  def addSignedHeader(signedHeader: SignedHeader): UntrustedState = {
    require(pending.isEmpty || (signedHeader.header.height < pending.head.header.height))
    UntrustedState(signedHeader :: pending)
  }.ensuring { res =>
    res.pending.head.header.height == signedHeader.header.height
  }

  def size: BigInt = pending.size
}

object UntrustedState {
  def empty: UntrustedState = UntrustedState(Nil[SignedHeader]())

  def apply(signedHeader: SignedHeader): UntrustedState = UntrustedState(Cons(signedHeader, Nil()))

  @scala.annotation.tailrec
  private def pendingInvariant(pending: List[SignedHeader]): Boolean = {
    pending match {
      case Cons(first, tail) if tail.isInstanceOf[Cons[SignedHeader]] =>
        first.header.height < tail.head.header.height && pendingInvariant(tail)
      case _ => true
    }
  }
}
