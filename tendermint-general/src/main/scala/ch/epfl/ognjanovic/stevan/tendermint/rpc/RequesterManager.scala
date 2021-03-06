package ch.epfl.ognjanovic.stevan.tendermint.rpc

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Address

/**
 * Manages `Requester`s for different full nodes identified by their network address.
 */
trait RequesterManager {

  /**
   * Returns a requester for a given address.
   *
   * @param address in a network of a full node
   */
  def requester(address: Address)
}
