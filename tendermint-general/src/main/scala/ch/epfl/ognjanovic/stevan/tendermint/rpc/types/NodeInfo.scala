package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.circe.ByteArray

case class NodeInfo(
  protocol_version: ProtocolVersion,
  id: ByteArray,
  listen_addr: String,
  network: String,
  version: String,
  channels: Long,
  moniker: String,
  other: OtherInfo)
