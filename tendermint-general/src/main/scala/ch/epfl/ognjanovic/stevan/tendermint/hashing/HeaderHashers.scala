package ch.epfl.ognjanovic.stevan.tendermint.hashing

import java.nio.ByteBuffer

import ch.epfl.ognjanovic.stevan.tendermint.merkle.MerkleRoot
import ch.epfl.ognjanovic.stevan.tendermint.rpc.circe.circe.ByteArray
import ch.epfl.ognjanovic.stevan.tendermint.verified.types.Header
import com.google.protobuf.timestamp.Timestamp
import com.google.protobuf.{ByteString, CodedOutputStream}
import tendermint.proto.types.types.{BlockID, PartSetHeader}
import tendermint.proto.version.version.Consensus

import scala.collection.mutable

object HeaderHashers {
  trait HeaderHasher {
    def hashHeader(header: Header): ByteArray
  }

  sealed class DefaultHeaderHasher(private val merkleRoot: MerkleRoot) extends HeaderHasher {
    override def hashHeader(header: Header): ByteArray = {
      val fieldsBytes: mutable.Queue[Array[Byte]] = mutable.Queue()
      fieldsBytes.enqueue(
        Consensus(header.version.block, header.version.app).toByteArray,
        encodeByteVector(ByteBuffer.wrap(header.chainId.getBytes)),
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
        encodeByteVector(ByteBuffer.wrap(BigInt(header.proposer.address, 16).toByteArray))
      )

      println(fieldsBytes.toList.map(_.toList.map(_.toInt & 0xff)))

      ByteBuffer.wrap(merkleRoot.computeRoot(fieldsBytes.toArray)).asReadOnlyBuffer()
    }

    private def extractBlockID(header: Header): BlockID = {
      val hash = ByteString.copyFrom(header.lastBlockId.bytes)
      val optionalPartSet = Some(
        PartSetHeader(
          header.lastBlockId.parts.total,
          ByteString.copyFrom(header.lastBlockId.parts.hash.array())))
      BlockID(hash, optionalPartSet)
    }

    private def encodeByteVector(bytes: ByteArray): Array[Byte] = {
      if (bytes.capacity() == 0)
        return Array.empty
      val output = Array.ofDim[Byte](CodedOutputStream.computeByteBufferSizeNoTag(bytes))
      val outputBuffer = CodedOutputStream.newInstance(output)
      outputBuffer.writeByteArrayNoTag(bytes.get(Array.ofDim[Byte](bytes.capacity())).array())
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
