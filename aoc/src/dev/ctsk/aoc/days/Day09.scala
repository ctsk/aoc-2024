package dev.ctsk.aoc.days

import dev.ctsk.aoc.*
import scala.annotation.tailrec
import scala.collection.mutable

object Day09 extends Solver(9):
  implicit def ord: Ordering[Record] = Ordering.by(-1 * _.offset)
  case class Record(offset: Int, size: Int, id: Int):
    def checksum: Long = id.toLong * (size * offset + size * (size - 1) / 2)

  case class GapMap(data: Map[Int, mutable.PriorityQueue[Record]]):
    def relocate(record: Record): Record =
      val bucket = data.view
        .filter((gapSize, gaps) => gapSize >= record.size && gaps.nonEmpty)
        .maxByOption(_._2.head)

      bucket match
        case None => record
        case Some((gapSize, gaps)) =>
          val gap = gaps.dequeue()
          if gap.offset >= record.offset then return record
          val rest = gapSize - record.size
          if rest > 0 then
            data(rest).enqueue(Record(gap.offset + record.size, rest, -1))
          Record(gap.offset, record.size, record.id)

  def part1(input: Array[Int]): Long =
    val materialized = input.zipWithIndex.flatMap((x, i) =>
      if i % 2 == 0
      then Iterator.fill(x)(i / 2)
      else Iterator.fill(x)(-1)
    )

    @tailrec
    def compact(l: Int = 0, r: Int = materialized.length - 1): Unit =
      if l >= r then return ()
      if materialized(l) != -1 then return compact(l + 1, r)
      if materialized(r) == -1 then return compact(l, r - 1)
      materialized(l) = materialized(r); materialized(r) = -1
      compact(l + 1, r - 1)

    compact()
    materialized.zipWithIndex.filter(_._1 != -1).map(_.toLong * _.toLong).sum

  def part2(input: Array[Int]): Long =
    val offsets = input.scanLeft(0)(_ + _).toArray

    val (gaps, files) = input.zip(offsets).zipWithIndex.partitionMap {
      case ((value, offset), index) =>
        if index % 2 == 0
        then Right(Record(offset, value, index / 2))
        else Left(Record(offset, value, -1))
    }

    val gapMap = GapMap(
      gaps.groupBy(_.size).view.mapValues(mutable.PriorityQueue.from).toMap
    )

    files.reverse.map(gapMap.relocate).map(_.checksum).sum

  def run(input: os.ReadablePath): (Timings, Solution) =
    val disk = os.read.lines(input)(0).map(_.asDigit).toArray
    val (p1_time, p1_solution) = timed { part1(disk) }
    val (p2_time, p2_solution) = timed { part2(disk) }

    (
      Timings(0, p1_time, p2_time),
      Solution(Long.box(p1_solution), Long.box(p2_solution))
    )
