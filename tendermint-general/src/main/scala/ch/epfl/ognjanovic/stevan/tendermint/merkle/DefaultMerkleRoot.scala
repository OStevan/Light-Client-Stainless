package ch.epfl.ognjanovic.stevan.tendermint.merkle

import java.security.MessageDigest

private[merkle] sealed class DefaultMerkleRoot(private val messageDigest: MessageDigest) extends MerkleRoot {
  override def computeRoot(items: Array[Array[Byte]]): Array[Byte] = {
    computeRootInner(items)
  }

  def computeSplitPoint(length: Int): Int = length match {
    case 0 | 1 => throw new IllegalArgumentException("Split point for numbers less than 2 does not make sense")
    case _ if Integer.bitCount(length) == 1 => length / 2
    case _ => 1 << (32 - Integer.numberOfLeadingZeros(length) - 1)
  }

  def computeRootInner(items: Array[Array[Byte]]): Array[Byte] = items.length match {
    case 0 => Array.empty
    case 1 => leafHash(items(0))
    case length =>
      val splitPoint = computeSplitPoint(length)
      val left = computeRootInner(items.slice(0, splitPoint))
      val right = computeRootInner(items.slice(splitPoint, length))
      innerHash(left, right)
  }

  def leafHash(bytes: Array[Byte]): Array[Byte] = {
    messageDigest.digest(0.toByte +: bytes)
  }

  def innerHash(left: Array[Byte], right: Array[Byte]): Array[Byte] = {
    messageDigest.digest(
      Array.concat(
        Array(1.toByte),
        left,
        right))
  }
}
