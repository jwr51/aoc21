package advent

import scala.annotation.tailrec
import scala.io.Source

object Day10 {

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day10.txt").getLines().toSeq
    val analyses = input.map(analyse)

    println(p1(analyses))
    println(p2(analyses))
  }

  type Analysis = Either[Char, Seq[Char]]
  case class Chunk(open: Char, close: Char, mismatchScore: Int, unclosedScore: Int)
  val chunks = Map(
    '(' -> Chunk('(', ')', mismatchScore = 3, unclosedScore = 1),
    '[' -> Chunk('[', ']', mismatchScore = 57, unclosedScore = 2),
    '{' -> Chunk('{', '}', mismatchScore = 1197, unclosedScore = 3),
    '<' -> Chunk('<', '>', mismatchScore = 25137, unclosedScore = 4)
  )

  def analyse(s: String): Analysis = {
    @tailrec
    def fn(todo: List[Char], open: List[Char]): Analysis = todo match {
      case Nil     => Right(open)
      case x :: xs =>
        if open.isEmpty then fn(xs, x :: open)
        else chunks.values.find(_.close == x) match {
          case None    => fn(xs, x :: open)
          case Some(c) =>
            if c.open != open.head then Left(x)
            else fn(xs, open.tail)
        }
    }

    fn(s.toList, List.empty)
  }

  def error(c: Char): Int = chunks.values.find(_.close == c).get.mismatchScore

  def close(open: Seq[Char]): Long =
    open.map(chunks).map(_.unclosedScore).foldLeft(0L)((acc, sc) => acc * 5 + sc)

  def p1(input: Seq[Analysis]): Long = input.collect({ case Left(c) => error(c) }).sum

  def p2(input: Seq[Analysis]): Long = {
    val scores = input.collect({ case Right(o) => o }).map(close).sorted
    scores(scores.length / 2)
  }
}
