package advent

import scala.annotation.tailrec
import scala.io.Source

object Day24:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day24.txt").getLines().toList
    val relations = associate(input)

    println(p1(relations))
    println(p2(relations))

  def associate(input: List[String]): Set[Relation] =
    @tailrec
    def fn(stack: List[(Int, Int)], lines: List[List[String]], rels: Set[Relation]): Set[Relation] =
      lines match
        case Nil     => rels
        case x :: xs =>
          val i = 13 - xs.size
          // added to popped value (min. 1), if a > 8 then cmp with input is always false
          val a = x(5).split(" ").last.toInt
          if a > 9 then
            val add = x(15).split(" ").last.toInt // added before pushing to stack
            fn((i, add) :: stack, xs, rels)
          else
            val (ci, ca) :: rest = stack
            val rel = Relation(i, a + ca, ci)
            fn(rest, xs, rels + rel)

    fn(List.empty, input.grouped(18).toList, Set.empty)

  case class Relation(dst: Int, delta: Int, src: Int):
    def sub: Relation = if delta < 0 then this else Relation(src, -delta, dst)
    def add: Relation = if delta > 0 then this else Relation(src, -delta, dst)

  def p1(input: Set[Relation]): Long =
    val digits = Array.fill(input.size * 2)(9)
    input.map(_.sub).foreach(r => digits.update(r.dst, digits(r.src) + r.delta))
    digits.mkString.toLong

  def p2(input: Set[Relation]): Long =
    val digits = Array.fill(input.size * 2)(1)
    input.map(_.add).foreach(r => digits.update(r.dst, digits(r.src) + r.delta))
    digits.mkString.toLong