package advent

import scala.io.Source

object Day06:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day06.txt").mkString.split(",").map(_.toInt).toSeq

    println(p1(input))
    println(p2(input))

  def population(input: Seq[Int], days: Int): Long =
    def evolve(state: List[Long]): List[Long] =
      val spawns :: tail = state
      tail.updated(6, tail(6) + spawns) :+ spawns

    val state: List[Long] = List.tabulate(9)(i => input.count(_ == i))
    Iterator.iterate(state)(evolve).drop(days).next.sum

  def p1(input: Seq[Int]): Long = population(input, days = 80)

  def p2(input: Seq[Int]): Long = population(input, days = 256)