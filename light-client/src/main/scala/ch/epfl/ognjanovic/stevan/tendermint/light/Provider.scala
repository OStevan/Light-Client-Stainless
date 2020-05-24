package ch.epfl.ognjanovic.stevan.tendermint.light

import ch.epfl.ognjanovic.stevan.tendermint.verified.types.LightBlock

trait Provider {
  /**
   * For a given height gives back the `LightBlock` of that height.
   *
   * @param height of the block, or 0 for the latest block
   * @return block for the specified height
   */
  def lightBlock(height: Long): LightBlock
}
