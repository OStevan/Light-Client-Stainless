package ch.epfl.ognjanovic.stevan.tendermint.light

object LightBlockStatuses {
  sealed trait LightBlockStatus

  object Verified extends LightBlockStatus

  object Trusted extends LightBlockStatus
}
