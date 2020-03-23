package ch.epfl.ognjanovic.stevan.types

import stainless.lang._
import stainless.annotation._
import stainless.collection._

object Chain {

  /**
   * By design chain can not be empty.
   */
  sealed abstract class Chain {
    @induct
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

    def appendBlock(blockHeader: BlockHeader): Chain = {
      require(blockHeader.height == this.height + 1 && blockHeader.validatorSet == head.nextValidatorSet)
      ChainLink(blockHeader, this)
    } ensuring(res => res.height == this.height + 1)
  }

  case class Genesis(blockHeader: BlockHeader) extends Chain {
    require(blockHeader.height == Height(1))
  }

  case class ChainLink(blockHeader: BlockHeader, tail: Chain) extends Chain {
    require(
      blockHeader.height == tail.height + 1 && // height needs to be increasing in steps of 1
        blockHeader.validatorSet == tail.head.nextValidatorSet // the link needs to be trusted
    )
  }
}
