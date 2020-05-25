package ch.epfl.ognjanovic.stevan.tendermint.verified.types

object BlockIdFlags {

  def apply(value: Byte): BlockIdFlag = {
    require(value > 0 && value <= 3)
    value.toInt match {
      case 1 => BlockIDFlagAbsent
      case 2 => BlockIDFlagCommit
      case 3 => BlockIdFlagNil
    }
  }

  sealed abstract class BlockIdFlag

  case object BlockIDFlagAbsent extends BlockIdFlag

  case object BlockIDFlagCommit extends BlockIdFlag

  case object BlockIdFlagNil extends BlockIdFlag

}
