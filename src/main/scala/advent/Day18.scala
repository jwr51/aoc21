package advent

import cats.parse.Numbers.digits
import cats.parse.Parser as P

import scala.annotation.tailrec
import scala.io.Source

object Day18:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day18.txt")
                      .getLines()
                      .map(node.parseAll)
                      .map {
                        case Left(e)  => throw new UnsupportedOperationException(e.toString)
                        case Right(p) => p
                      }.toList

    println(p1(input))
    println(p2(input))

  sealed trait Node:
    val magnitude: Int = this match
      case Leaf(value)       => value
      case Pair(left, right) => left.magnitude * 3 + right.magnitude * 2

    def leftAdd(addend: Int): Node = this match
      case Leaf(value)       => Leaf(value + addend)
      case Pair(left, right) => Pair(left.leftAdd(addend), right)

    def rightAdd(addend: Int): Node = this match
      case Leaf(value)       => Leaf(value + addend)
      case Pair(left, right) => Pair(left, right.rightAdd(addend))

    def +(other: Node): Node = Pair(this, other).reduce

    def reduce: Node = explode.orElse(split).map(_.reduce).getOrElse(this)

    def explode: Option[Node] =
      def fn(node: Node, depth: Int): Option[(Option[Int], Node, Option[Int])] =
        node match
          case Pair(Leaf(left), Leaf(right)) if depth >= 4 =>
            Some(Some(left), Leaf(0), Some(right))
          case Pair(left, right)                           =>
            fn(left, depth + 1).map((leftVal, left, rightVal) =>
              (leftVal, Pair(left, rightVal.map(right.leftAdd).getOrElse(right)), None)
            ).orElse(fn(right, depth + 1).map((lv, right, rv) =>
              (None, Pair(lv.map(left.rightAdd).getOrElse(left), right), rv))
            )
          case Leaf(_)                                     =>
            None

      fn(this, 0).map((_, n, _) => n)

    def split: Option[Node] =
      this match
        case Leaf(value) if value >= 10 =>
          val half = value / 2F
          Some(Pair(Leaf(half.floor.toInt), Leaf(half.ceil.toInt)))
        case Pair(left, right)          =>
          left.split.map(Pair(_, right)).orElse(right.split.map(Pair(left, _)))
        case Leaf(_)                    =>
          None

  case class Leaf(value: Int) extends Node
  case class Pair(left: Node, right: Node) extends Node

  def node: P[Node] = P.recursive[Node] { recurse =>
    val leaf: P[Leaf] = digits.map(n => Leaf(n.toInt))
    val pair: P[Pair] = recurse.repSep(2, 2, P.char(','))
                               .between(P.char('['), P.char(']'))
                               .map(ns => Pair(ns.head, ns.last))

    P.oneOf(leaf :: pair :: Nil)
  }

  def p1(input: List[Node]): Long = input.reduce(_ + _).magnitude

  def p2(input: List[Node]): Long =
    input.combinations(2).map(_.reduce(_ + _).magnitude).max