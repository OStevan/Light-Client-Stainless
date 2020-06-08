package ch.epfl.ognjanovic.stevan.tendermint.merkle

import java.security.MessageDigest

trait MerkleRoot {
  def computeRoot(items: Array[Array[Byte]]): Array[Byte]
}

object MerkleRoot {
  val default: MerkleRoot = new DefaultMerkleRoot(MessageDigest.getInstance("SHA-256"))

  def apply(messageDigest: MessageDigest): MerkleRoot = {
    require(messageDigest.getAlgorithm == "SHA-256", "unsupported algorithm")
    new DefaultMerkleRoot(messageDigest)
  }
}
