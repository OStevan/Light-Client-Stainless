package ch.epfl.ognjanovic.stevan.tendermint.rpc

import ch.epfl.ognjanovic.stevan.tendermint.rpc.Requester.{SignedHeaderNotFoundException, ValidatorSetNotFoundException}
import ch.epfl.ognjanovic.stevan.tendermint.rpc.types.{Address, SignedHeader, ValidatorSet}

/**
 * Minimal set of methods required to communicate with Tendermint full node to be able to do Light Client verification
 */
trait Requester {
  /**
   * Returns the string representation of the chain id.
   *
   * @return
   */
  def chainId(): String

  /**
   * Requests `SignedHeader` of a specific height.
   *
   * @param height of the block we are interested in, needs to be non negative value, for 0 the latest header is
   *               returned
   * @return header of the specified height, or the latest one height is zero
   * @throws SignedHeaderNotFoundException when the header of a specified height is not found
   * @throws IllegalArgumentException      when height is negative
   */
  def signedHeader(height: Long): SignedHeader

  /**
   * Requests `ValidatorSet` of a specific height.
   *
   * @param height of the header for which we want to get the `ValidatorSet` needs to be non negative value, for 0 the
   *               validator set of the latest header is returned
   * @return `Validator` set for the specified height
   * @throws ValidatorSetNotFoundException when the requested set is not found
   * @throws IllegalArgumentException      when height is negative
   */
  def validatorSet(height: Long): ValidatorSet

  /**
   * Address of the peer this requester is querying.
   *
   * @return address of the peer
   */
  def peer: Address
}

object Requester {

  case class SignedHeaderNotFoundException() extends RuntimeException("Signed header not found!!!")

  case class ValidatorSetNotFoundException() extends RuntimeException("Validator set not found")

}
