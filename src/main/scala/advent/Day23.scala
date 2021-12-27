package advent

import scala.annotation.tailrec
import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.io.Source
import scala.math.*

object Day23:

  def main(args: Array[String]): Unit =
    val input = Source.fromResource("day23.txt").getLines().toList
    println(arrange(parse(input)))

    val hidden = List("  #D#C#B#A#", "  #D#B#A#C#")
    val extra = input.take(3) ::: hidden ::: input.drop(3)
    println(arrange(parse(extra)))

  private val indexOf = Map('A' -> 2, 'B' -> 4, 'C' -> 6, 'D' -> 8)
  private val costOf = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
  private val hallway = (0 to 10).filter(h => !indexOf.values.exists(_ == h))

  case class Burrow(hall: List[Char], rooms: Map[Char, List[Char]]):
    def arranged: Boolean = rooms.forall((kind, room) => room.forall(_ == kind))

    def moves: Iterable[(Burrow, Int)] = roomsToHall ++ hallToRooms

    def roomsToHall: Iterable[(Burrow, Int)] =
      for
        (kind, room) <- rooms
        if room.exists(c => c != kind && c != '.')
        pos <- reachable(indexOf(kind))
      yield roomToHall(kind, pos)

    def reachable(from: Int): Iterable[Int] =
      hallway.filter(to => (min(from, to) to max(from, to)).filter(_ != from).forall(hall(_) == '.'))

    def roomToHall(kind: Char, pos: Int): (Burrow, Int) =
      val room = rooms(kind)
      val top = room.indexWhere(_.isLetter)

      val newHall = hall.updated(pos, room(top))
      val newRooms = rooms.updated(kind, room.updated(top, '.'))
      val dist = (indexOf(kind) - pos).abs + top + 1
      (Burrow(newHall, newRooms), dist * costOf(room(top)))

    def hallToRooms: Iterable[(Burrow, Int)] =
      hall.zipWithIndex.filter((c, _) => c.isLetter).flatMap(hallToRoom.tupled)

    def hallToRoom(kind: Char, pos: Int): Option[(Burrow, Int)] =
      if rooms(kind).exists(c => c != kind && c != '.') then return None

      val idx = indexOf(kind)
      if !reachable(pos).exists(_ == idx + (pos - idx).sign) then return None

      val room = rooms(kind)
      val top = room.lastIndexOf('.')

      val newHall = hall.updated(pos, '.')
      val newRooms = rooms.updated(kind, room.updated(top, kind))
      val dist = (idx - pos).abs + top + 1
      Some((Burrow(newHall, newRooms), dist * costOf(kind)))

  case class State(burrow: Burrow, cost: Int)

  def parse(xs: List[String]): Burrow =
    def room(x: Int) = xs.map(_ (x)).filter(_.isLetter)
    Burrow(List.fill(11)('.'), indexOf.map((c, x) => c -> room(x + 1)))

  def arrange(burrow: Burrow): Int =
    val state = State(burrow, 0)
    val costs = mutable.Map(burrow -> 0)
    val queue = mutable.PriorityQueue(state)(Ordering.by[State, Int](_.cost).reverse)

    while (queue.nonEmpty) {
      val State(b, cost) = queue.dequeue()
      if b.arranged then return cost

      b.moves
       .filter((next, dist) => !costs.get(next).exists(_ <= (cost + dist)))
       .foreach { (next, dist) =>
         val sum = cost + dist
         costs.update(next, sum)
         queue.enqueue(State(next, sum))
       }
    }

    throw new IllegalArgumentException("No solution.")
