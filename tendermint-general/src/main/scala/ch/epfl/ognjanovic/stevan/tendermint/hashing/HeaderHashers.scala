package ch.epfl.ognjanovic.stevan.tendermint.hashing

import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.circe.ByteArray
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Header
import com.google.protobuf.timestamp.Timestamp
import com.google.protobuf.{ByteString, CodedOutputStream}
import tendermint.proto.types.types.{BlockID, PartSetHeader}
import tendermint.proto.version.version.Consensus

object HeaderHashers {
  trait HeaderHasher {
    def hashHeader(header: Header): ByteArray
  }

  sealed class DefaultHeaderHasher(private val merkleRoot: MerkleRoot) extends HeaderHasher {
    override def hashHeader(header: Header): ByteArray = {
      val fieldsBytes = Array[Array[Byte]](
        Consensus(header.version.block, header.version.app).toByteArray,
        encodeByteVector(header.chainId.getBytes),
        encodeVarInt(header.height.value.toLong),
        Timestamp(header.time.getEpochSecond, header.time.getNano).toByteArray,
        extractBlockID(header).toByteArray,
        encodeByteVector(header.lastCommit),
        encodeByteVector(header.data),
        encodeByteVector(header.validators),
        encodeByteVector(header.nextValidators),
        encodeByteVector(header.consensus),
        encodeByteVector(header.app),
        encodeByteVector(header.lastResults),
        encodeByteVector(header.evidence),
        encodeByteVector(header.proposer.address)
      )

      merkleRoot.computeRoot(fieldsBytes).toVector
    }

    private def extractBlockID(header: Header): BlockID = {
      BlockID(
        ByteString.copyFrom(header.lastBlockId.bytes.toArray),
        Some(
          PartSetHeader(
            header.lastBlockId.parts.total,
            ByteString.copyFrom(header.lastBlockId.parts.hash.toArray))))
    }

    private def encodeByteVector(bytes: ByteArray): Array[Byte] = {
      if (bytes.isEmpty)
        return Array.empty
      val byteString = ByteString.copyFrom(bytes.toArray)
      val output = Array.ofDim[Byte](CodedOutputStream.computeBytesSizeNoTag(byteString))
      val outputBuffer = CodedOutputStream.newInstance(output)
      outputBuffer.writeBytesNoTag(byteString)
      output
    }

    private def encodeVarInt(long: Long): Array[Byte] = {
      val output = Array.ofDim[Byte](CodedOutputStream.computeInt64SizeNoTag(long))
      val outputBuffer = CodedOutputStream.newInstance(output)
      outputBuffer.writeInt64NoTag(long)
      output
    }
  }
}
