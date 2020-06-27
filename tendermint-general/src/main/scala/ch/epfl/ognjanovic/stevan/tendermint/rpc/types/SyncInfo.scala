package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import java.time.Instant

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.circe.ByteArray

case class SyncInfo(
  latest_block_hash: ByteArray,
  latest_app_hash: ByteArray,
  latest_block_height: Long,
  latest_block_time: Instant,
  catching_up: Boolean)
