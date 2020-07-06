package ch.epfl.ognjanovic.stevan.tendermint.light.store

import ch.epfl.ognjanovic.stevan.tendermint.light.LightBlockStatuses.LightBlockStatus
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.{Height, LightBlock}

// ported from tendermint-rs
/**
 * Implementations need to be stateful.
 */
trait LightStore {
  def get(height: Height, status: LightBlockStatus): Option[LightBlock]

  def update(lightBlock: LightBlock, status: LightBlockStatus)

  def remove(height: Height)

  def latest(status: LightBlockStatus): Option[LightBlock]

  def all(status: LightBlockStatus): Iterable[LightBlock]
}
