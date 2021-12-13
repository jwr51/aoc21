package advent

import scala.annotation.tailrec
import scala.io.Source

object Day12:

  def main(args: Array[String]): Unit =
    val Pattern = """(\w+)-(\w+)""".r
    val caves: Caves =
      Source
        .fromResource("day12.txt")
        .getLines()
        .toList
        .flatMap { case Pattern(a, b) => Set(a -> b, b -> a) }
        .groupMapReduce(_._1)(p => Set(p._2))(_ ++ _)

    println(p1(caves))
    println(p2(caves))

  type Cave = String
  type Caves = Map[Cave, Set[Cave]]
  type Path = List[Cave]

  extension (cave: Cave)
    def small: Boolean = cave.head.isLower
    def large: Boolean = cave.head.isUpper

  def paths(allow: (Cave, Path) => Boolean)(caves: Caves): Set[Path] =
    def paths(p: Path, ps: Set[Path]): Set[Path] =
      caves(p.head).flatMap(_ match
        case "end" => ps + p.reverse
        case c     => if allow(c, p) then paths(c :: p, ps) else ps
      )

    paths(List("start"), Set.empty)

  def p1(caves: Caves): Int = paths((c, p) => c.large || !p.contains(c))(caves).size

  def p2(caves: Caves): Int =
    def filter(c: Cave, p: Path): Boolean =
      if c.large || !p.contains(c) then true
      else if c == "start" then false
      else
        val fs = p.filter(_.small).groupMapReduce(identity)(_ => 1)(_ + _)
        fs(c) < (if !fs.values.exists(_ == 2) then 2 else 1)

    paths(filter)(caves).size
