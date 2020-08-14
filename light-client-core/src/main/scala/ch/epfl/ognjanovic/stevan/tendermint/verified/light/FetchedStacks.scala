package ch.epfl.ognjanovic.stevan.tendermint.verified.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}
import stainless.annotation.pure
import stainless.collection.{Cons, List}
import stainless.lang._

object FetchedStacks {

  @scala.annotation.tailrec
  private def pendingInvariant(pending: List[LightBlock]): Boolean = {
    pending match {
      case Cons(first, tail) if tail.isInstanceOf[Cons[LightBlock]] =>
        first.header.height < tail.head.header.height && pendingInvariant(tail)
      case _ => true
    }
  }

  abstract class FetchedStack {

    def targetLimit: Height = {
      ??? : Height
    }

    @pure
    def pop(): (LightBlock, FetchedStack) = {
      require(peek().isDefined)
      ??? : (LightBlock, FetchedStack)
    }.ensuring(res =>
      res._2.targetLimit == targetLimit &&
        res._1 == peek().get &&
        res._2.peek().map(peek().get.header.height < _.header.height).getOrElse(true))

    @pure
    def push(lightBlock: LightBlock): FetchedStack = {
      require(
        peek().map(lightBlock.header.height < _.header.height).getOrElse(true) &&
          lightBlock.header.height <= targetLimit)
      ??? : FetchedStack
    }.ensuring(res =>
      res.targetLimit == targetLimit &&
        res.peek().isDefined &&
        peek().isDefined ==> res.peek().get.header.height < peek().get.header.height &&
        res.peek().get == lightBlock)

    @pure
    def peek(): Option[LightBlock] = {
      ??? : Option[LightBlock]
    }.ensuring(res => res.map(_.header.height <= targetLimit).getOrElse(true))

  }

  case class InMemoryFetchedStack(override val targetLimit: Height, pending: List[LightBlock]) extends FetchedStack {
    require(pendingInvariant(pending) && pending.forall(_.header.height <= targetLimit))

    @pure
    override def pop(): (LightBlock, FetchedStack) = {
      require(peek().isDefined)
      (pending.head, InMemoryFetchedStack(targetLimit, pending.tail))
    }

    @pure
    override def push(lightBlock: LightBlock): FetchedStack = {
      require(
        peek().map(lightBlock.header.height < _.header.height).getOrElse(true)
          && lightBlock.header.height <= targetLimit)
      InMemoryFetchedStack(targetLimit, lightBlock :: pending)
    }.ensuring(res =>
      res.targetLimit == targetLimit &&
        res.peek().isDefined &&
        peek().isDefined ==> res.peek().get.header.height < peek().get.header.height &&
        res.peek().get == lightBlock)

    @pure
    override def peek(): Option[LightBlock] = {
      if (pending.nonEmpty)
        Some(pending.head)
      else
        None[LightBlock]()
    }

  }

}
