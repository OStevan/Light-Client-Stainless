package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.pure
import stainless.collection.{Cons, List}
import stainless.lang._

object UntrustedStates {

  @scala.annotation.tailrec
  private def pendingInvariant(pending: List[LightBlock]): Boolean = {
    pending match {
      case Cons(first, tail) if tail.isInstanceOf[Cons[LightBlock]] =>
        first.header.height < tail.head.header.height && pendingInvariant(tail)
      case _ => true
    }
  }

  @pure
  def empty(targetHeight: Height): UntrustedState = {
    InMemoryUntrustedState(targetHeight, List.empty)
  }.ensuring(res => res.bottomHeight().isEmpty && targetHeight == res.targetLimit)

  abstract class UntrustedState {
    def targetLimit: Height = {
      ??? : Height
    }.ensuring(res => bottomHeight().forall(_ <= res))

    @pure
    def hasNextHeader(bottom: Height, target: Height): Boolean = {
      require(bottom < target && bottomHeight().map(bottom < _).getOrElse(true) && target <= targetLimit)
      ??? : Boolean
    }.ensuring(res =>
      (res && bottomHeight().isDefined && bottom < bottomHeight().get && bottomHeight().get <= target) ||
        (!res && bottomHeight().map(target < _).getOrElse(true) && (target == targetLimit) ==> bottomHeight().isEmpty))

    @pure
    def removeBottom(): (LightBlock, UntrustedState) = {
      require(bottomHeight().isDefined)
      ??? : (LightBlock, UntrustedState)
    }.ensuring(res =>
      res._2.targetLimit == targetLimit &&
        res._1.header.height == bottomHeight().get &&
        res._2.bottomHeight().map(bottomHeight().get < _).getOrElse(true))

    @pure
    def insertLightBlock(lightBlock: LightBlock): UntrustedState = {
      require(
        bottomHeight().map(lightBlock.header.height < _).getOrElse(true) &&
          lightBlock.header.height <= targetLimit)
      ??? : UntrustedState
    }.ensuring(res =>
      res.targetLimit == targetLimit &&
        res.bottomHeight().isDefined &&
        bottomHeight().isDefined ==> res.bottomHeight().get < bottomHeight().get &&
        res.bottomHeight().get == lightBlock.header.height)

    @pure
    def bottomHeight(): Option[Height] = {
      ??? : Option[Height]
    }.ensuring(res => res.map(_ <= targetLimit).getOrElse(true))
  }

  case class InMemoryUntrustedState(
    override val targetLimit: Height,
    pending: List[LightBlock]) extends UntrustedState {
    require(pendingInvariant(pending) && pending.forall(_.header.height <= targetLimit))

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
    override def removeBottom(): (LightBlock, UntrustedState) = {
      require(bottomHeight().isDefined)
      (pending.head, InMemoryUntrustedState(targetLimit, pending.tail))
    }

    @pure
    override def insertLightBlock(lightBlock: LightBlock): UntrustedState = {
      require(
        bottomHeight().map(lightBlock.header.height < _).getOrElse(true)
          && lightBlock.header.height <= targetLimit)
      InMemoryUntrustedState(targetLimit, lightBlock :: pending)
    }.ensuring(res =>
      res.targetLimit == targetLimit &&
        res.bottomHeight().isDefined &&
        bottomHeight().isDefined ==> res.bottomHeight().get < bottomHeight().get &&
        res.bottomHeight().get == lightBlock.header.height)

    @pure
    override def bottomHeight(): Option[Height] = {
      if (pending.nonEmpty)
        Some(pending.head.header.height)
      else
        None[Height]()
    }
  }

}
