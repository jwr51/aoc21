package advent

import scala.io.Source

object Day14:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day14.txt").getLines().toList
    val polymer = input.head
    val rules = input.drop(2)
                     .map(_.split(" -> "))
                     .map { case Array(a, b) => a -> b }
                     .toMap

    println(p1(polymer, rules))
    println(p2(polymer, rules))

  def toPairs(polymer: String): Map[String, Long] =
    polymer.sliding(2).toList.groupMapReduce(identity)(_ => 1L)(_ + _)

  def iterate(polymer: String, rules: Map[String, String], steps: Int): Long =
    def step(pairs: Map[String, Long]): Map[String, Long] =
      pairs.toList
           .flatMap((p, n) => Seq(p.head + rules(p) -> n, rules(p) + p.last -> n))
           .groupMapReduce((p, _) => p)((_, n) => n)(_ + _)

    val ps = Iterator.iterate(toPairs(polymer))(step).drop(steps).next()
    // account for last element of polymer
    val es = (ps + ((polymer.last + "?") -> 1L)).groupMapReduce((p, _) => p.head)((_, n) => n)(_ + _)
    es.values.max - es.values.min

  def p1(polymer: String, rules: Map[String, String]): Long = iterate(polymer, rules, steps = 10)

  def p2(polymer: String, rules: Map[String, String]): Long = iterate(polymer, rules, steps = 40)