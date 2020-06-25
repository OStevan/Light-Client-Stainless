package ch.epfl.ognjanovic.stevan.tendermint.rpc

import ch.epfl.ognjanovic.stevan.tendermint.rpc.Requester.{SignedHeaderNotFoundException, ValidatorSetNotFoundException}
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, PeerId, ValidatorSet}
import stainless.annotation.ignore

/**
 * Minimal set of methods required to communicate with Tendermint full node to be able to do Light Client verification
 */
trait Requester {

  /**
   * Requests `SignedHeader` of a specific height.
   *
   * @param height of the block we are interested in, if `None` is supplied the latest `SignedHeader` is returned
   * @return header of the specified height, or the latest one
   * @throws SignedHeaderNotFoundException when the header of a specified height is not found
   * @throws IllegalArgumentException      when height is negative
   */
  def signedHeader(height: Option[Height]): SignedHeader

  /**
   * Requests `ValidatorSet` of a specific height.
   *
   * @param height of the header for which we want to get the `ValidatorSet`, when `None` is supplied the `ValidatorSet`
   *               of the latest one is returned
   * @return `ValidatorSet` for the specified height
   * @throws ValidatorSetNotFoundException when the requested set is not found
   * @throws IllegalArgumentException      when height is negative
   */
  def validatorSet(height: Option[Height]): ValidatorSet

  /**
   * PeerId of the full node.
   *
   * @return PeerId of the node
   */
  def peerId: PeerId
}

@ignore
object Requester {

  case class SignedHeaderNotFoundException() extends RuntimeException("Signed header not found!!!")

  case class ValidatorSetNotFoundException() extends RuntimeException("Validator set not found")

}
