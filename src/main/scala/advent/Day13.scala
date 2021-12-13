package advent

import scala.io.Source

object Day13:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day13.txt").getLines().toList
    val paper = input
      .takeWhile(_.nonEmpty)
      .map(_.split(","))
      .map(_.map(_.toInt))
      .map { case Array(x, y) => Point(x, y) }
      .toSet

    val folds = input
      .drop(paper.size + 1)
      .map(_.split("="))
      .map { case Array(f, n) => if f.last == 'x' then FoldLeft(n.toInt) else FoldUp(n.toInt) }

    println(p1(paper, folds))
    println(p2(paper, folds))

  sealed trait Fold
  case class FoldLeft(x: Int) extends Fold
  case class FoldUp(y: Int) extends Fold

  type Paper = Set[Point]

  extension (paper: Paper)
    def exec(fold: Fold): Paper = paper.map(_.fold(fold))

  case class Point(x: Int, y: Int) {
    def fold(fold: Fold): Point =
      fold match
        case FoldLeft(c) => if x > c then copy(x = c - (x - c)) else this
        case FoldUp(c)   => if y > c then copy(y = c - (y - c)) else this
  }

  def p1(paper: Paper, folds: Seq[Fold]): Int = paper.exec(folds.head).size

  def p2(paper: Paper, folds: Seq[Fold]): String =
    val r = folds.foldLeft(paper)((p, f) => p.exec(f))

    val max = Point(r.map(_.x).max + 1, r.map(_.y).max + 1)
    List.tabulate(max.y)(y => List.tabulate(max.x)(x => if r.contains(Point(x, y)) then '#' else '.'))
        .map(_.mkString)
        .mkString("\n")
