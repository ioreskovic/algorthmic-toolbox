package io.ioreskovic.coursera.w4

import scala.io.StdIn

object QuickSort3 {
  def main(args: Array[String]): Unit = {
    StdIn.readLine()
    val input = StdIn.readLine().split(" ").map(_.toInt)
    val result = sort(input)(select(_))
    println(result.mkString(" "))
  }

  def sort[T](seq: Seq[T])(sel: Seq[T] => T)(implicit ord: T => Ordered[T]): Seq[T] = {
    if (seq.length <= 1) seq
    else {
      val (lt, et, gt) = partition(seq, sel(seq))
      val lts = sort(lt)(sel)
      val rts = sort(gt)(sel)
      lts ++ et ++ rts
    }
  }

  def partition[T](seq: Seq[T], pivot: T)(implicit ord: T => Ordered[T]): (Seq[T], Seq[T], Seq[T]) = {
    val (lte, gt) = seq.partition(_ <= pivot)
    val (lt, et) = lte.partition(_ < pivot)
    (lt, et, gt)
  }

  def select[T](seq: Seq[T])(implicit ord: T => Ordered[T]): T = {
    val a = seq.head
    val b = seq.last
    val c = seq(seq.length / 2)
    median3(a, b, c)
  }

  def median3[T](a: T, b: T, c: T)(implicit ord: T => Ordered[T]): T = {
    if ((a <= b) && (a <= c)) {
      if (b < c) b
      else c
    } else if ((b <= a) && (b <= c)) {
      if (a < c) a
      else c
    } else {
      if (a < b) a
      else b
    }
  }
}
