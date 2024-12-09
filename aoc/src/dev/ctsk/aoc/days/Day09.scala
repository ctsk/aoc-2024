package dev.ctsk.aoc.days

import dev.ctsk.aoc._
import scala.collection.mutable.PriorityQueue
import scala.math.Ordered.orderingToOrdered
import scala.annotation.tailrec

object Day09 extends Solver(9):
  implicit def ord: Ordering[Record] = Ordering.by(-_.offset)

  case class Record(offset: Int, size: Int, id: Int):
    def checksum: Long = id.toLong * (size * offset + size * (size - 1) / 2)

  case class GapMap(
      data: Map[Int, PriorityQueue[Record]]
  ):
    def relocate(record: Record): Record =
      val bucket = data.view
        .filter((gapSize, gaps) => gapSize >= record.size && gaps.nonEmpty)
        .minByOption(_._2.head.offset)

      bucket match
        case None => record
        case Some((gapSize, gaps)) =>
          val gap = gaps.dequeue()
          if gap.offset >= record.offset then return record
          val remaining = gapSize - record.size
          if remaining > 0 then
            data(remaining)
              .enqueue(Record(gap.offset + record.size, remaining, gap.id))
          Record(gap.offset, record.size, record.id)

  def part1(input: Array[Int]): Long =
    val materialized = input.zipWithIndex.flatMap((x, i) =>
      if i % 2 == 0 then Iterator.fill(x.toInt)(i / 2)
      else Iterator.fill(x.toInt)(-1)
    )

    @tailrec def compact(l: Int = 0, r: Int = materialized.length - 1): Unit =
      if l >= r then return ()
      if materialized(l) != -1 then return compact(l + 1, r)
      if materialized(r) == -1 then return compact(l, r - 1)
      materialized(l) = materialized(r); materialized(r) = -1
      compact(l + 1, r - 1)

    compact()
    materialized.zipWithIndex.filter(_._1 != -1).map(_.toLong * _.toLong).sum

  def part2(input: Array[Int]): Long =
    val offsets = input.scanLeft(0)(_ + _).toArray
    val records =
      input.zipWithIndex.zip(offsets).map { case ((value, index), offset) =>
        if index % 2 == 0
        then Record(offset, value, index / 2)
        else Record(offset, value, -1)
      }

    val (gaps, files) = records.partition(_.id == -1)
    val gapMap = GapMap(
      gaps.groupBy(_.size).mapValues(PriorityQueue.from(_)).toMap
    )

    files.reverse.map(gapMap.relocate(_)).map(_.checksum).sum

  def run(input: os.ReadablePath): (Timings, Solution) =
    val disk = os.read.lines(input)(0).map(_.asDigit).toArray
    val (p1_time, p1_solution) = timed { part1(disk) }
    val (p2_time, p2_solution) = timed { part2(disk) }

    (
      Timings(0, p1_time, p2_time),
      Solution(Long.box(p1_solution), Long.box(p2_solution))
    )
