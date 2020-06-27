package ch.epfl.ognjanovic.stevan.tendermint.rpc.types

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.ValidatorSet

object Responses {
  case class JsonRpcResponse[T](jsonrpc: String, id: Int, result: T)

  case class CommitResponse(signed_header: SignedHeader, canonical: Boolean)

  case class ValidatorSetResponse(block_height: Int, validators: ValidatorSet, count: Int, total: Int)

  case class StatusResponse(node_info: NodeInfo, sync_info: SyncInfo, validator_info: ValidatorInfo)
}
