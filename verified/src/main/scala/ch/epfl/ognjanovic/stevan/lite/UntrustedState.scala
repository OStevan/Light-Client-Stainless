package ch.epfl.ognjanovic.stevan.lite

import ch.epfl.ognjanovic.stevan.types.SignedHeader
import stainless.lang._
import stainless.collection._
import stainless.annotation._

case class UntrustedState(pending: List[SignedHeader]) {
  require(UntrustedState.pendingInvariant(pending))

  def addSignedHeader(signedHeader: SignedHeader): UntrustedState = pending match {
    case Nil() => UntrustedState(Cons(signedHeader, Nil()))
    case Cons(h, _) => 
    if (signedHeader.header.height < h.header.height)
      UntrustedState(Cons(signedHeader, pending))
    else
      this
  }

  def removeHead: (Option[SignedHeader], UntrustedState) = pending match {
    case Cons(h, t) => (Some(h), UntrustedState(t))
    case Nil() => (None(), this)
  }
}

object UntrustedState {
  private def pendingInvariant(pending: List[SignedHeader]): Boolean = pending match {
    case Cons(first, Cons(second, tail)) => first.header.height < second.header.height && pendingInvariant(Cons(second, tail))
    case _ => true
  }
}