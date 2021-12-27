package advent

import scala.annotation.tailrec
import scala.io.Source

object Day19:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day19.txt")
                      .mkString
                      .split("\n\n")
                      .toSeq
                      .map(_.split("\n").tail.toSeq)
                      .map(parse)

    val (scanner, points) = resolve(input)
    println(scanner.beacons.size)
    println(points.toSeq.combinations(2).map(ps => ps.head manhattan ps.last).max)

  def align(s1: Scanner, candidate: Scanner): Option[(Scanner, Vec3)] =
    candidate
      .orientations
      .view
      .filter(s2 => (s1.offsets & s2.offsets).size >= 12)
      .flatMap { s2 =>
        for
          b1 <- s1.beacons
          b2 <- s2.beacons
          dist = b1 - b2
          translated = s2.beacons.map(_ + dist)
          if (s1.beacons & translated).size >= 12
        yield Some((s2, dist))
      }.flatten
      .headOption

  def resolve(scanners: Iterable[Scanner]): (Scanner, Set[Vec3]) =
    @tailrec
    def fn(acc: Scanner, rem: Set[Scanner], points: Set[Vec3]): (Scanner, Set[Vec3]) =
      if rem.isEmpty then return (acc, points)

      val (scanner, (aligned, point)) = rem.view.flatMap(s2 => align(acc, s2).map((s2, _))).head
      val beacons = acc.beacons ++ aligned.beacons.map(_ + point)
      fn(Scanner(beacons), rem - scanner, points + point)

    fn(scanners.head, scanners.tail.toSet, Set.empty)

  case class Scanner(beacons: Set[Vec3]):
    val offsets: Set[Vec3] =
      beacons
        .toSeq
        .combinations(2)
        .map(_.reduce(_ dist _))
        .toSet

    def orientations: Iterable[Scanner] =
      beacons.map(_.orientations)
             .transpose
             .map(Scanner.apply)

  def parse(input: Seq[String]): Scanner =
    val beacons = input.map(_.split(",").map(_.toInt))
                       .map { m => Vec3(m.head, m(1), m(2)) }
    Scanner(beacons.toSet)

  val rotations: Seq[Mat3] =
    val X = Mat3(Vec3(1, 0, 0), Vec3(0, 0, -1), Vec3(0, 1, 0))
    val Y = Mat3(Vec3(0, 0, 1), Vec3(0, 1, 0), Vec3(-1, 0, 0))

    val a = for
      p <- 0 to 3
      q <- 0 to 3
      r <- 0 to 3
      s <- 0 to 3
    yield (X ^ p) * (Y ^ q) * (X ^ r) * (Y ^ s)
    a.distinct

  case class Vec3(x: Int, y: Int, z: Int):
    def dot(other: Vec3): Int = x * other.x + y * other.y + z * other.z
    def dist(other: Vec3): Vec3 = Vec3((x - other.x).abs, (y - other.y).abs, (z - other.z).abs)
    def +(other: Vec3): Vec3 = Vec3(x + other.x, y + other.y, z + other.z)
    def -(other: Vec3): Vec3 = Vec3(x - other.x, y - other.y, z - other.z)
    def manhattan(other: Vec3): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
    def orientations: Iterable[Vec3] = rotations.map(_ * this)

  case class Mat3(r0: Vec3, r1: Vec3, r2: Vec3):
    def c0: Vec3 = Vec3(r0.x, r1.x, r2.x)
    def c1: Vec3 = Vec3(r0.y, r1.y, r2.y)
    def c2: Vec3 = Vec3(r0.z, r1.z, r2.z)

    def *(other: Mat3): Mat3 =
      Mat3(
        Vec3(r0 dot other.c0, r0 dot other.c1, r0 dot other.c2),
        Vec3(r1 dot other.c0, r1 dot other.c1, r1 dot other.c2),
        Vec3(r2 dot other.c0, r2 dot other.c1, r2 dot other.c2)
      )

    def *(col: Vec3): Vec3 = Vec3(r0 dot col, r1 dot col, r2 dot col)

    def ^(pow: Int): Mat3 =
      pow match
        case 0 => Mat3(Vec3(1, 0, 0), Vec3(0, 1, 0), Vec3(0, 0, 1))
        case 1 => this
        case _ => (this ^ (pow - 1)) * this