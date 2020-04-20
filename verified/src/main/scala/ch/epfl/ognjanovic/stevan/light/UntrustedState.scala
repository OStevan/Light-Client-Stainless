package ch.epfl.ognjanovic.stevan.light

import ch.epfl.ognjanovic.stevan.types.SignedHeader
import stainless.collection._
import stainless.lang._

case class UntrustedState(pending: List[SignedHeader]) {
  require(UntrustedState.pendingInvariant(pending))

  def addSignedHeader(signedHeader: SignedHeader): UntrustedState = {
      require(complexRequire(signedHeader))
      pending match {
      case Nil() => UntrustedState(Cons(signedHeader, Nil()))
      case Cons(h, _) =>
        UntrustedState(Cons(signedHeader, pending))
    }
  }

  def removeHead: (Option[SignedHeader], UntrustedState) = pending match {
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

  private def pendingInvariant(pending: List[SignedHeader]): Boolean = pending match {
    case Cons(first, Cons(second, tail)) => first.header.height < second.header.height && pendingInvariant(Cons(second, tail))
    case _ => true
  }
}