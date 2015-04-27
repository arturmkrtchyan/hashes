package com.arturmkrtchyan.hashes.noncrypto

import org.specs2.mutable._
import com.google.common.hash.Hashing

import scala.util.Random

object MurmurHash3Spec extends Specification {

  "32 bit MurmurHash3" should {

    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    val bytes = (alphabet + alphabet.toUpperCase).getBytes
    val r = Random

    (0 until 20).foreach(index => {
      val bytes2pick = r.nextInt(bytes.length - 1) + 1
      val bytes2hash = bytes.zipWithIndex
        .filter(byte => byte._2 < bytes2pick)
        .map(byte => byte._1)

      val str2hash = new String(bytes2hash)
      s"be equal to guava murmur3 given [seed: $index, str: $str2hash]" in {
        val guavaHash = Hashing.murmur3_32(index).hashBytes(bytes2hash).asInt
        val myHash = MurmurHash3.hash32(bytes2hash, index)

        myHash mustEqual guavaHash
      }

    })
  }



}
