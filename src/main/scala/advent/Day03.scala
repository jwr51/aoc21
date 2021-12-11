package advent

import scala.annotation.tailrec
import scala.io.Source

object Day03 {

  // https://adventofcode.com/2021/day/3
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day03.txt").getLines().toSeq
    
    println(p1(input))
    println(p2(input))
  }

  def mode(xs: Seq[Char]): Char = if xs.count(_ == '1') >= xs.length / 2D then '1' else '0'

  def antiMode(xs: Seq[Char]): Char = if mode(xs) == '1' then '0' else '1'

  def atoi2(s: String): Int = Integer.parseInt(s, 2)

  def p1(input: Seq[String]): Int = {
    val cols = input.transpose
    val modes = cols.map(mode)

    val gamma = atoi2(modes.mkString)
    val epsilon = (~gamma) & ((1 << cols.length) - 1)

    gamma * epsilon
  }

  def p2(input: Seq[String]): Int = {
    def select(fn: Seq[Char] => Char)(xs: Seq[String]): String = {
      @tailrec
      def _select(xs: Seq[String], i: Int = 0): String = xs match {
        case x :: Nil => x
        case _        =>
          val c = fn(xs.transpose.apply(i))
          _select(xs.filter(_ (i) == c), i + 1)
      }

      _select(xs)
    }

    atoi2(select(mode)(input)) * atoi2(select(antiMode)(input))
  }
}
