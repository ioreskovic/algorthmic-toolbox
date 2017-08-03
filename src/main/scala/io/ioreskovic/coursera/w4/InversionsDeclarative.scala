package io.ioreskovic.coursera.w4

import scala.annotation.tailrec
import scala.io.StdIn

object InversionsDeclarative {
  case class ListInv[T](seq: List[T], inversions: Long) {
    def length: Int = seq.length
    def halves: (ListInv[T], ListInv[T]) = {
      val (l, r) = seq.splitAt(seq.length / 2)
      (ListInv(l, 0), ListInv(r, 0))
    }
    def apply(n: Int): T = seq(n)
  }

  def main(args: Array[String]): Unit = {
    StdIn.readLine()
    val input = StdIn.readLine().split(" ").map(_.toInt).toList
    val result = sort(ListInv(input, 0))
    println(result.inversions)
  }

  def sort[T](seq: ListInv[T])(implicit ord: T => Ordered[T]): ListInv[T] = {
    if (seq.length < 2) ListInv(seq.seq, 0)
    else {
      val (l, r) = seq.halves
      val (ls, rs) = (sort(l), sort(r))
      merge(ls, rs, ListInv(Nil, 0))
    }
  }

  @tailrec
  def merge[T](aSeq: ListInv[T], bSeq: ListInv[T], res: ListInv[T])(implicit ord: T => Ordered[T]): ListInv[T] = (aSeq.seq, bSeq.seq) match {
    case (Nil, Nil) => ListInv(res.seq.reverse, res.inversions + aSeq.inversions + bSeq.inversions)
    case (a :: as, b :: bs) if a <= b => merge(ListInv(as, aSeq.inversions), bSeq, ListInv(a :: res.seq, res.inversions))
    case (a :: as, b :: bs) => merge(aSeq, ListInv(bs, bSeq.inversions), ListInv(b :: res.seq, res.inversions + aSeq.length))
    case (a :: as, Nil) => merge(ListInv(as, aSeq.inversions), bSeq, ListInv(a :: res.seq, res.inversions))
    case (Nil, b :: bs) => merge(aSeq, ListInv(bs, bSeq.inversions), ListInv(b :: res.seq, res.inversions))
  }
}
