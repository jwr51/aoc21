package advent

import scala.io.Source

object Day07 {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day07.txt").mkString.split(",").map(_.toInt).toSeq
    
    println(p1(input))
    println(p2(input))
  }

  def fuel(cs: Seq[Int])(cost: Int => Int): Int =
    (cs.min to cs.max).map(c => cs.map(d => cost((c - d).abs)).sum).min

  def p1(crabs: Seq[Int]): Int = fuel(crabs)(identity)

  def p2(crabs: Seq[Int]): Int = fuel(crabs)(n => n * (n + 1) / 2)
}