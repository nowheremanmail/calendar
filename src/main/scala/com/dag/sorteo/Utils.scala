package com.dag.sorteo

import java.security.SecureRandom

import scala.collection.BuildFrom
import scala.collection.mutable.ArrayBuffer

object Utils {
  var sr = new SecureRandom

  def nextInt(i: Int) = sr.nextInt(i)

  def shuffle[T, C](xs: IterableOnce[T])(implicit bf: BuildFrom[xs.type, T, C]): C = {
    val buf = new ArrayBuffer[T] ++= xs

    def swap(i1: Int, i2: Int): Unit = {
      val tmp = buf(i1)
      buf(i1) = buf(i2)
      buf(i2) = tmp
    }

    for (n <- buf.length to 2 by -1) {
      val k = nextInt(n)
      swap(n - 1, k)
    }

    (bf.newBuilder(xs) ++= buf).result()
  }

}
