package advent

import scala.annotation.tailrec

object Day3 {

  // https://adventofcode.com/2021/day/3
  def main(args: Array[String]): Unit = {
    val input = Advent.input(3)
    println(p1(input))
    println(p2(input))
  }

  def mode(xs: List[Char]): Char = if (xs.count(_ == '1') >= xs.length / 2D) '1' else '0'

  def atoi2(s: String): Int = Integer.parseInt(s, 2)

  def p1(input: String): Any = {
    val cols = input.linesIterator.toList.transpose
    val modes = cols.map(mode)

    val gamma = atoi2(modes.mkString)
    val epsilon = (~gamma) & ((1 << (cols.length - 1)) - 1)

    gamma * epsilon
  }

  def p2(input: String): Any = {
    def select(fn: List[Char] => Char)(xs: List[String]): String = {
      @tailrec
      def _select(xs: List[String], i: Int = 0): String = xs match {
        case x :: Nil => x
        case _        =>
          val c = fn(xs.transpose.apply(i))
          _select(xs.filter(_ (i) == c), i + 1)
      }

      _select(xs)
    }

    val lines = input.linesIterator.toList

    val o2 = select(mode)(lines)
    val co2 = select(xs => if (mode(xs) == '1') '0' else '1')(lines)

    atoi2(o2) * atoi2(co2)
  }
}
