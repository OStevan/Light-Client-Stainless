package ch.epfl.ognjanovic.stevan.types

import stainless.annotation._
import stainless.collection._
import stainless.lang._

object Chain {

  /**
   * By design chain can not be empty.
   */
  sealed abstract class Chain {
    def forAll(condition: BlockHeader => Boolean): Boolean = this match {
      case ChainLink(head, tail) => condition(head) && tail.forAll(condition)
      case Genesis(blockHeader) => condition(blockHeader)
    }

    def size: BigInt = {
      this match {
        case Genesis(_) => BigInt(1)
        case ChainLink(_, tail) => BigInt(1) + tail.size
      }
    }.ensuring((res: BigInt) => res > BigInt(0) && res == head.height.value)

    def height: Height = Height(size)

    def map[T](f: BlockHeader => T): List[T] = {
      this match {
        case Genesis(blockHeader) => List(f(blockHeader))
        case ChainLink(blockHeader, tail) => f(blockHeader) :: tail.map(f)
      }
    }.ensuring(res => res.size == this.size)

    def head: BlockHeader = this match {
      case Genesis(blockHeader) => blockHeader
      case ChainLink(blockHeader, _) => blockHeader
    }

    @inline
    def appendBlock(blockHeader: BlockHeader): Chain = {
      require(
        blockHeader.height == this.height + 1 &&
          blockHeader.validatorSet == head.nextValidatorSet &&
          blockHeader.nextValidatorSet.keys.nonEmpty)
      ChainLink(blockHeader, this)
    } ensuring (res => res.height == this.height + 1)
  }

  case class Genesis(blockHeader: BlockHeader) extends Chain {
    require(blockHeader.height == Height(1) && blockHeader.nextValidatorSet.keys.nonEmpty)
  }

  case class ChainLink(blockHeader: BlockHeader, tail: Chain) extends Chain {
    require(
      blockHeader.height == tail.height + 1 && // height needs to be increasing in steps of 1
        blockHeader.validatorSet == tail.head.nextValidatorSet && // the link needs to be trusted
        blockHeader.nextValidatorSet.keys.nonEmpty
    )
  }
}
