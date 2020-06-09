package ch.epfl.ognjanovic.stevan.tendermint.merkle

import java.security.MessageDigest

import org.scalatest.funsuite.AnyFunSuite
import stainless.annotation.ignore

@ignore
sealed class DefaultMerkleRootTests extends AnyFunSuite {

  test("Split point computations should be valid") {
    val merkleRoot = new DefaultMerkleRoot(MessageDigest.getInstance("SHA-256"))
    assert(merkleRoot.computeSplitPoint(2) == 1)
    assert(merkleRoot.computeSplitPoint(3) == 2)
    assert(merkleRoot.computeSplitPoint(4) == 2)
    assert(merkleRoot.computeSplitPoint(5) == 4)
    assert(merkleRoot.computeSplitPoint(10) == 8)
    assert(merkleRoot.computeSplitPoint(20) == 16)
    assert(merkleRoot.computeSplitPoint(100) == 64)
    assert(merkleRoot.computeSplitPoint(255) == 128)
    assert(merkleRoot.computeSplitPoint(256) == 128)
    assert(merkleRoot.computeSplitPoint(257) == 256)
  }

  test("RFC6962 empty leaf computation test") {
    val merkleRoot = new DefaultMerkleRoot(MessageDigest.getInstance("SHA-256"))
    val emptyLeafRoot: Array[Byte] =
      Array(
        0x6e, 0x34, 0x0b, 0x9c, 0xff, 0xb3, 0x7a, 0x98, 0x9c, 0xa5, 0x44, 0xe6, 0xbb, 0x78, 0x0a, 0x2c,
        0x78, 0x90, 0x1d, 0x3f, 0xb3, 0x37, 0x38, 0x76, 0x85, 0x11, 0xa3, 0x06, 0x17, 0xaf, 0xa0, 0x1d).map(_.toByte)

    val emptyTree = Array(Array[Byte]())
    assert(emptyLeafRoot.deep == merkleRoot.computeRoot(emptyTree).deep)
  }

  test("RFC6962 leaf computation for L123456") {
    val merkleRoot = new DefaultMerkleRoot(MessageDigest.getInstance("SHA-256"))
    val leafRoot: Array[Byte] =
      Array(
        0x39, 0x5a, 0xa0, 0x64, 0xaa, 0x4c, 0x29, 0xf7, 0x01, 0x0a, 0xcf, 0xe3, 0xf2, 0x5d, 0xb9, 0x48,
        0x5b, 0xbd, 0x4b, 0x91, 0x89, 0x7b, 0x6a, 0xd7, 0xad, 0x54, 0x76, 0x39, 0x25, 0x2b, 0x4d, 0x56).map(_.toByte)
    val leafString = "L123456"

    val leafTree = Array(leafString.getBytes)
    assert(leafRoot.deep == merkleRoot.computeRoot(leafTree).deep)
  }

  test("RFC6962 node computation for left \"N123\" and right \"N456\"") {
    val merkleRoot = new DefaultMerkleRoot(MessageDigest.getInstance("SHA-256"))
    val nodeHash: Array[Byte] =
      Array(
        0xaa, 0x21, 0x7f, 0xe8, 0x88, 0xe4, 0x70, 0x07, 0xfa, 0x15, 0xed, 0xab, 0x33, 0xc2, 0xb4, 0x92,
        0xa7, 0x22, 0xcb, 0x10, 0x6c, 0x64, 0x66, 0x7f, 0xc2, 0xb0, 0x44, 0x44, 0x4d, 0xe6, 0x6b, 0xbb).map(_.toByte)
    val leftNode = "N123"
    val rightNode = "N456"

    assert(nodeHash.deep == merkleRoot.innerHash(leftNode.getBytes, rightNode.getBytes()).deep)
  }

}
