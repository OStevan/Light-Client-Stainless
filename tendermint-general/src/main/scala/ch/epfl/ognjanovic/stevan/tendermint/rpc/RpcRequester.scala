package ch.epfl.ognjanovic.stevan.tendermint.rpc
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, PeerId, ValidatorSet}

class RpcRequester(val peerId: PeerId, nodeClient: TendermintFullNodeClient) extends Requester {
  override def signedHeader(height: Option[Height]): SignedHeader = nodeClient.commit(height)

  override def validatorSet(height: Option[Height]): ValidatorSet = nodeClient.validatorSet(height)
}
