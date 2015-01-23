package com.arturmkrtchyan.hashes.noncrypto

//
// https://code.google.com/p/smhasher/wiki/MurmurHash3
//
// https://code.google.com/p/smhasher/source/browse/trunk/MurmurHash3.cpp
//

import java.lang.Integer.{ rotateLeft => rotl }
import java.lang.Long.{ rotateLeft => rotl64 }

private[noncrypto] class MurmurHash3_x86_32 extends HashFunction {

  private val c1 = 0xcc9e2d51
  private val c2 = 0x1b873593

  def hash(data: Array[Byte], seed: Int): Int = {
    var length = data.length
    var hash = seed
    val blockSize = 4

    // Body
    var i = 0
    while(length >= blockSize) {

      // read 4 byte key to int
      var key = readInt(data, i)

      key = mixKey(key)
      hash = mixHash(hash, key)

      i += blockSize
      length -= blockSize
    }

    // Tail
    var key = 0
    if(length == 3) key ^= (data(i + 2) & 0xFF) << 16
    if(length >= 2) key ^= (data(i + 1) & 0xFF) << 8
    if(length >= 1) {
      key ^= (data(i + 0) & 0xFF)
      hash ^= mixKey(key)
    }

    finalizeHash(hash, data.length)
  }

  private def readInt(data: Array[Byte], index: Int): Int = {
    // read 4 bytes key to int (little-endian)
    var key = data(index + 0) & 0xFF
    key |= (data(index + 1) & 0xFF) << 8
    key |= (data(index + 2) & 0xFF) << 16
    key |= (data(index + 3) & 0xFF) << 24
    key
  }

  private def mixKey(key: Int): Int = {
    var k = key
    k *= c1
    k = rotl(k, 15)
    k *= c2
    k
  }

  private def mixHash(hash: Int, key: Int): Int = {
    var h = hash
    h ^= key
    h = rotl(h, 13)
    h = h * 5 + 0xe6546b64
    h
  }

  /** Finalizing the hash - force all bits of the hash block to avalanche */
  private def finalizeHash(hash: Int, length: Int): Int = {
    var h = hash
    h ^= length
    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16
    h
  }
}

private[noncrypto] class MurmurHash3_x64_128 extends HashFunction {

  private def c1 = 0x87c37b91114253d5L
  private def c2 = 0x4cf5ad432745937fL

  def hash(data: Array[Byte], seed: Int): Int = {
    var length = data.length
    var hash1:Long = seed
    var hash2:Long = seed
    val blockSize = 16

    // Body
    var i = 0
    while(length >= blockSize) {

      // read 8 byte keys
      var key1 = readLong(data, i)
      var key2 = readLong(data, i + 8)

      // FIXME extract to small functions
      key1 *= c1; key1  = rotl64(key1,31); key1 *= c2; hash1 ^= key1

      hash1 = rotl64(hash1,27); hash1 += hash2; hash1 = hash1 * 5 + 0x52dce729

      key2 *= c2; key2  = rotl64(key2,33); key2 *= c1; hash2 ^= key2

      hash2 = rotl64(hash2,31); hash2 += hash1; hash2 = hash2 * 5 + 0x38495ab5

      i += blockSize
      length -= blockSize
    }

    // TODO handle tail

    var k1 = 0L
    var k2 = 0L

    0
  }

  private def readLong(data: Array[Byte], index: Int): Long = {
    // read 8 byte key to long key
    var key = data(index + 0) & 0xFF
    key |= (data(index + 1) & 0xFF) << 8
    key |= (data(index + 2) & 0xFF) << 16
    key |= (data(index + 3) & 0xFF) << 24
    key |= (data(index + 4) & 0xFF) << 32
    key |= (data(index + 5) & 0xFF) << 40
    key |= (data(index + 6) & 0xFF) << 48
    key |= (data(index + 7) & 0xFF) << 56
    key
  }


  /** Finalizing the hash - force all bits of the hash block to avalanche */
  private def finalizeHash(hash1: Long, hash2: Long, length: Int): (Long, Long) = {
    var h1 = hash1
    var h2 = hash2

    h1 ^= length
    h2 ^= length

    h1 += h2
    h2 += h1

    h1 = finalizationMix(h1)
    h2 = finalizationMix(h2)

    h1 += h2
    h2 += h1


    (h1, h2)
  }

  private def finalizationMix(hash: Long): Long = {
    var h = hash
    h ^= h >> 33
    h *= 0xff51afd7ed558ccdL
    h ^= h >> 33
    h *= 0xc4ceb9fe1a85ec53L
    h ^= h >> 33
    h
  }

}

object MurmurHash3 extends HashFunction {

  private val hasher32 = new MurmurHash3_x86_32()
  private val hasher128 = new MurmurHash3_x64_128()

  def hash32(data: Array[Byte], seed: Int): Int = {
    hasher32.hash(data, seed)
  }

  def hash128(): Array[Byte] = {
    Array.emptyByteArray
  }

}