package advent

import cats.parse.Parser0 as P0
import cats.parse.Parser as P

import scala.io.Source

object Day16:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day16.txt").mkString
    val packet = decode(input)

    println(p1(packet))
    println(p2(packet))

  sealed trait Packet:
    val ver: Int

    def verSum: Int = this match
      case Literal(ver, _)       => ver
      case Operator(ver, _, ops) => ver + ops.map(_.verSum).sum

    def eval: Long = this match
      case Literal(_, value)          => value
      case Operator(_, 0, ops)        => ops.map(_.eval).sum
      case Operator(_, 1, ops)        => ops.map(_.eval).product
      case Operator(_, 2, ops)        => ops.map(_.eval).min
      case Operator(_, 3, ops)        => ops.map(_.eval).max
      case Operator(_, 5, List(a, b)) => if a.eval > b.eval then 1 else 0
      case Operator(_, 6, List(a, b)) => if a.eval < b.eval then 1 else 0
      case Operator(_, 7, List(a, b)) => if a.eval == b.eval then 1 else 0
      case _                          => assert(false)

  case class Literal(ver: Int, value: Long) extends Packet
  case class Operator(ver: Int, kind: Int, operands: List[Packet]) extends Packet

  extension (s: String)
    def toInt2: Int = Integer.parseInt(s, 2)

  def packet: P[Packet] =
    for
      ver <- int2(3)
      kind <- int2(3)
      p <- if kind == 4 then literal(ver) else operator(ver, kind)
    yield p

  def literal(ver: Int): P[Packet] = number().map(Literal(ver, _))

  def number(num: Long = 0): P[Long] =
    for
      continue <- bool
      bits <- int2(4)
      value = (num << 4) | bits
      result <- if continue then number(value) else P.pure(value)
    yield result

  def operator(ver: Int, kind: Int): P[Packet] =
    for
      mode <- bool
      count <- int2(if mode then 11 else 15)
      operands <- operands(mode, count)
    yield Operator(ver, kind, operands)

  def operands(mode: Boolean, count: Int): P0[List[Packet]] =
    if mode then packet.repExactlyAs[List[Packet]](count)
    else for
      end <- index
      ps <- packet.repUntil(index.filter(_ == end + count))
    yield ps.toList

  def index: P0[Int] = P.index

  def bool: P[Boolean] = take(1).map(_ == "1")

  def int2(bits: Int): P[Int] = take(bits).map(_.toInt2)

  def take(bits: Int): P[String] =
    P.charIn("01")
     .repExactlyAs[List[Char]](bits)
     .map(_.mkString)

  def decode(s: String): Packet =
    val bin = s.map(_.asDigit)
               .map(_.toBinaryString.toInt)
               .map(n => "%04d".format(n))
               .mkString

    packet.parse(bin) match
      case Left(e)       => throw UnsupportedOperationException(e.toString)
      case Right((r, p)) => if !r.contains("1") then p else throw new IllegalStateException("Unread chars.")

  def p1(packet: Packet): Int = packet.verSum

  def p2(packet: Packet): Long = packet.eval