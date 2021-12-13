package advent

import scala.io.Source

object Day02:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day02.txt").getLines().map(parse).toSeq

    println(p1(input))
    println(p2(input))

  sealed trait Command
  case class MoveHorizontal(n: Int) extends Command
  case class MoveVertical(n: Int) extends Command

  private val Pattern = """(\w+) (\d+)""".r

  def parse(s: String): Command = s match
    case Pattern("forward", n) => MoveHorizontal(n.toInt)
    case Pattern("down", n)    => MoveVertical(n.toInt)
    case Pattern("up", n)      => MoveVertical(-n.toInt)

  def p1(cs: Seq[Command]): Int =
    val (dist, depth) = cs.foldLeft((0, 0)) { case ((dist, depth), c) =>
      c match
        case MoveHorizontal(n) => (dist + n, depth)
        case MoveVertical(n)   => (dist, depth + n)
    }
    dist * depth

  def p2(cs: Seq[Command]): Int =
    val (dist, depth, _) = cs.foldLeft((0, 0, 0)) { case ((dist, depth, aim), c) =>
      c match
        case MoveHorizontal(n) => (dist + n, depth + aim * n, aim)
        case MoveVertical(n)   => (dist, depth, aim + n)
    }
    dist * depth