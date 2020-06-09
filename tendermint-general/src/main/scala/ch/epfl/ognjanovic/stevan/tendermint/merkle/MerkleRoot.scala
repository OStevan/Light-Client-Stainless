package ch.epfl.ognjanovic.stevan.tendermint.merkle

import java.security.MessageDigest

trait MerkleRoot {
  def computeRoot(items: Array[Array[Byte]]): Array[Byte]
}

object MerkleRoot {
  // do not make this a val as the MessageDigest instances are not
  def default(): MerkleRoot = new DefaultMerkleRoot(MessageDigest.getInstance("SHA-256"))

  def apply(messageDigest: MessageDigest): MerkleRoot = {
    require(messageDigest.getAlgorithm == "SHA-256", "unsupported algorithm")
    new DefaultMerkleRoot(messageDigest)
  }
}
