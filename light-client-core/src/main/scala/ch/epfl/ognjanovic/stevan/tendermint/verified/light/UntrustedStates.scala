package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.pure
import stainless.collection.List
import stainless.lang._

object UntrustedStates {

  @pure
  def empty(targetHeight: Height): AbstractUntrustedState = {
    UntrustedState(targetHeight, List.empty)
  }.ensuring(res => res.bottomHeight().isEmpty && targetHeight == res.targetLimit)

  abstract class AbstractUntrustedState {
    val targetLimit: Height

    @pure
    def isIntermediateFetched(bottom: Height, top: Height): Boolean = {
      require(bottom < top && bottomHeight().map(bottom < _).getOrElse(true) && top <= targetLimit)
      ??? : Boolean
    }.ensuring(res =>
      (res && bottomHeight().isDefined && bottom < bottomHeight().get && bottomHeight().get < top) ||
        (!res && bottomHeight().map(top < _).getOrElse(true) && (top == targetLimit) ==> bottomHeight().isEmpty))

    @pure
    def removeBottom(): (LightBlock, AbstractUntrustedState) = {
      require(bottomHeight().isDefined)
      ??? : (LightBlock, AbstractUntrustedState)
    }.ensuring(res =>
      res._2.targetLimit == targetLimit &&
        res._1.header.height == bottomHeight().get &&
        res._2.bottomHeight().map(bottomHeight().get < _).getOrElse(true))

    @pure
    def insertLightBlock(lightBlock: LightBlock): AbstractUntrustedState = {
      require(
        bottomHeight().map(lightBlock.header.height < _).getOrElse(true) &&
          lightBlock.header.height <= targetLimit)
      ??? : AbstractUntrustedState
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

  case class UntrustedState(targetLimit: Height, pending: List[LightBlock]) extends AbstractUntrustedState {
    require(HelperUntrustedState.pendingInvariant(pending) && pending.forall(_.header.height <= targetLimit))

    @pure
    override def isIntermediateFetched(bottom: Height, top: Height): Boolean = {
      require(bottom < top && bottomHeight().map(bottom < _).getOrElse(true) && top <= targetLimit)
      if (bottomHeight().isEmpty)
        false
      else {
        bottom < bottomHeight().get && bottomHeight().get < top
      }
    }

    @pure
    override def removeBottom(): (LightBlock, AbstractUntrustedState) = {
      require(bottomHeight().isDefined)
      (pending.head, UntrustedState(targetLimit, pending.tail))
    }

    @pure
    override def insertLightBlock(lightBlock: LightBlock): AbstractUntrustedState = {
      require(
        bottomHeight().map(lightBlock.header.height < _).getOrElse(true)
          && lightBlock.header.height <= targetLimit)
      UntrustedState(targetLimit, lightBlock :: pending)
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
