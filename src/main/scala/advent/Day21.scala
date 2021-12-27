package advent

import cats.implicits.*

import scala.annotation.tailrec
import scala.math.max

object Day21:

  def main(args: Array[String]): Unit = {
    val input = (6, 2)

    println(p1.tupled(input))
    println(p2.tupled(input))
  }

  def p1(p1: Int, p2: Int): Int =
    @tailrec
    def play(p1: Int, p2: Int, s1: Int, s2: Int, rolls: LazyList[Int], turn: Int): Int =
      if s1 >= 1000 then s2 * turn * 3
      else if s2 >= 1000 then s1 * turn * 3
      else
        val (r, t) = rolls.splitAt(3)
        val sum = r.sum
        if turn % 2 == 0 then
          val p = (p1 + sum) % 10
          play(p, p2, s1 + p + 1, s2, t, turn + 1)
        else
          val p = (p2 + sum) % 10
          play(p1, p, s1, s2 + p + 1, t, turn + 1)

    val rolls: LazyList[Int] = LazyList.continually(1 to 100).flatten
    play(p1 - 1, p2 - 1, 0, 0, rolls, 0)

  def memo[A, B](fn: A => B): A => B =
    val cache = collection.mutable.HashMap[A, B]()
    a => cache.getOrElseUpdate(a, fn(a))

  def p2(p1: Int, p2: Int): Long =
    val dirac = for r1 <- 1 to 3; r2 <- 1 to 3; r3 <- 1 to 3 yield r1 + r2 + r3

    lazy val play: ((Int, Int, Int, Int, Int)) => (Long, Long) = memo {
      (p1, p2, s1, s2, turn) =>
        if s1 >= 21 then (1L, 0L)
        else if s2 >= 21 then (0L, 1L)
        else if turn % 2 == 0 then
          dirac.map(r => (r + p1) % 10)
               .map(p => play(p, p2, s1 + p + 1, s2, turn + 1))
               .reduce(_ |+| _)
        else
          dirac.map(r => (r + p2) % 10)
               .map(p => play(p1, p, s1, s2 + p + 1, turn + 1))
               .reduce(_ |+| _)
    }

    val (w1, w2) = play(p1 - 1, p2 - 1, 0, 0, 0)
    max(w1, w2)