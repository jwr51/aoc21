package advent

import scala.annotation.tailrec
import scala.io.Source

object Day12 {

  def main(args: Array[String]): Unit = {
    val Pattern = """(\w+)-(\w+)""".r
    val graph: Graph =
      Source
        .fromResource("day12.txt")
        .getLines()
        .flatMap { case Pattern(a, b) => Seq(a -> b, b -> a) }
        .foldLeft(Map.empty)((es, e) => es.updated(e._1, es.getOrElse(e._1, Set.empty) + e._2))

    println(p1(graph))
    println(p2(graph))
  }

  type Graph = Map[String, Set[String]]
  type Path = List[String]

  def paths(fn: (String, Path) => Boolean)(g: Graph): Set[Path] = {
    def paths(p: Path, ps: Set[Path]): Set[Path] =
      g(p.head).flatMap(_ match {
        case "end" => ps + p
        case n     => if fn(n, p) then paths(n :: p, ps) else ps
      })

    paths(List("start"), Set.empty)
  }

  def p1(graph: Graph): Int = paths((n, p) => n.head.isUpper || !p.contains(n))(graph).size

  def p2(graph: Graph): Int = {
    def filter(n: String, p: Path): Boolean =
      if n.head.isUpper || !p.contains(n) then true
      else if n == "start" then false
      else {
        val freqs = p.filter(_.head.isLower).groupMapReduce(identity)(_ => 1)(_ + _)
        val permitted = if !freqs.values.exists(_ == 2) then 2 else 1
        freqs(n) < permitted
      }

    paths(filter)(graph).size
  }
}
