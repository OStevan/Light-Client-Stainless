package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Height
import stainless.annotation.pure
import stainless.collection.{Cons, List}
import stainless.lang._

object UntrustedTraces {

  @scala.annotation.tailrec
  private def pendingInvariant(pending: List[Height]): Boolean = {
    pending match {
      case Cons(first, tail) if tail.isInstanceOf[Cons[Height]] =>
        first < tail.head && pendingInvariant(tail)
      case _ => true
    }
  }

  abstract class UntrustedTrace {

    def targetLimit: Height = {
      ??? : Height
    }

    @pure
    def hasNextHeader(bottom: Height, target: Height): Boolean = {
      require(bottom < target && bottomHeight().map(bottom < _).getOrElse(true) && target <= targetLimit)
      ??? : Boolean
    }.ensuring(res =>
      (res && bottomHeight().isDefined && bottom < bottomHeight().get && bottomHeight().get <= target) ||
        (!res && bottomHeight().map(target < _).getOrElse(true) && (target == targetLimit) ==> bottomHeight().isEmpty))

    @pure
    def removeBottom(): (Height, UntrustedTrace) = {
      require(bottomHeight().isDefined)
      ??? : (Height, UntrustedTrace)
    }.ensuring(res =>
      res._2.targetLimit == targetLimit &&
        res._1 == bottomHeight().get &&
        res._2.bottomHeight().map(bottomHeight().get < _).getOrElse(true))

    @pure
    def insertLightBlock(height: Height): UntrustedTrace = {
      require(
        bottomHeight().map(height < _).getOrElse(true) &&
          height <= targetLimit)
      ??? : UntrustedTrace
    }.ensuring(res =>
      res.targetLimit == targetLimit &&
        res.bottomHeight().isDefined &&
        bottomHeight().isDefined ==> res.bottomHeight().get < bottomHeight().get &&
        res.bottomHeight().get == height)

    @pure
    def bottomHeight(): Option[Height] = {
      ??? : Option[Height]
    }.ensuring(res => res.map(_ <= targetLimit).getOrElse(true))

  }

  case class InMemoryUntrustedTrace(override val targetLimit: Height, pending: List[Height]) extends UntrustedTrace {
    require(pendingInvariant(pending) && pending.forall(_ <= targetLimit))

    @pure
    override def hasNextHeader(bottom: Height, top: Height): Boolean = {
      require(bottom < top && bottomHeight().map(bottom < _).getOrElse(true) && top <= targetLimit)
      if (bottomHeight().isEmpty)
        false
      else {
        bottom < bottomHeight().get && bottomHeight().get <= top
      }
    }

    @pure
    override def removeBottom(): (Height, UntrustedTrace) = {
      require(bottomHeight().isDefined)
      (pending.head, InMemoryUntrustedTrace(targetLimit, pending.tail))
    }

    @pure
    override def insertLightBlock(height: Height): UntrustedTrace = {
      require(
        bottomHeight().map(height < _).getOrElse(true)
          && height <= targetLimit)
      InMemoryUntrustedTrace(targetLimit, height :: pending)
    }.ensuring(res =>
      res.targetLimit == targetLimit &&
        res.bottomHeight().isDefined &&
        bottomHeight().isDefined ==> res.bottomHeight().get < bottomHeight().get &&
        res.bottomHeight().get == height)

    @pure
    override def bottomHeight(): Option[Height] = {
      if (pending.nonEmpty)
        Some(pending.head)
      else
        None[Height]()
    }

  }

}