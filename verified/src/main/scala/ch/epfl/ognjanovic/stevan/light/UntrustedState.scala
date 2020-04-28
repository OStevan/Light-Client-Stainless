package ch.epfl.ognjanovic.stevan.light

import ch.epfl.ognjanovic.stevan.types.SignedHeader.SignedHeader
import stainless.collection._
import stainless.lang._

case class UntrustedState(pending: List[SignedHeader]) {
  require(UntrustedState.pendingInvariant(pending))

  def addSignedHeader(signedHeader: SignedHeader): UntrustedState = {
    require(complexRequire(signedHeader))
    UntrustedState(signedHeader :: pending)
  }.ensuring { res =>
    res.pending.head.header.height == signedHeader.header.height
  }

  @inline
  def removeHead(): (Option[SignedHeader], UntrustedState) = pending match {
    case Cons(h, t) => (Some(h), UntrustedState(t))
    case Nil() => (None(), this)
  }

  private def complexRequire(signedHeader: SignedHeader): Boolean = {
    if (pending.isEmpty)
      true
    else {
      signedHeader.header.height < pending.head.header.height
    }
  }
}

object UntrustedState {
  def empty: UntrustedState = UntrustedState(Nil[SignedHeader]())

  def apply(signedHeader: SignedHeader): UntrustedState = UntrustedState(Cons(signedHeader, Nil()))

  @scala.annotation.tailrec
  private def pendingInvariant(pending: List[SignedHeader]): Boolean = pending match {
    case Cons(first, tail) if tail.isInstanceOf[Cons[SignedHeader]] =>
      first.header.height < tail.head.header.height && pendingInvariant(tail)
    case _ => true
  }
}
