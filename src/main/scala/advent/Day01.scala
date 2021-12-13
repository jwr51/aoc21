package advent

import scala.io.Source

object Day01:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day01.txt").getLines().map(_.toInt).toSeq

    println(p1(input))
    println(p2(input))

  def p1(input: Seq[Int]): Int = input.sliding(2).count(p => p.last > p.head)

  def p2(input: Seq[Int]): Int = input.sliding(4).count(p => p.last > p.head)
