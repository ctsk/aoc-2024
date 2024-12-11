package dev.ctsk.aoc.days

import dev.ctsk.aoc._
import scala.collection.mutable

object Day11 extends Solver(11):
  // upper and lower halves of number
  def halves(n: Long): Option[(Long, Long)] =
    val numDigits = (Math.log10(n) + 1).toLong
    numDigits % 2 match
      case 1 => None
      case 0 => {
        Some(
          (
            n % Math.pow(10, numDigits / 2).toLong,
            n / Math.pow(10, numDigits / 2).toLong
          )
        )
      }

  def counter_compute(stones: Array[Long], depth: Long): Long =
    var acc: mutable.Map[Long, Long] =
      mutable.Map.from(stones.map((_, 1L)))

    for (_ <- 0L until depth) {
      val next = mutable.Map.empty[Long, Long].withDefaultValue(0L)
      for ((stone, count) <- acc) {
        if stone == 0 then next(1) += count
        else
          halves(stone.toLong) match
            case Some((a, b)) =>
              next(a) += count
              next(b) += count
            case None =>
              next(stone * 2024) += count
      }
      acc = next
    }

    acc.values.sum

  def run(input: os.ReadablePath): (Timings, Solution) =
    val in = longs(os.read.lines(input).head).toArray

    val p1 = timed { counter_compute(in, 25) }
    val p2 = timed { counter_compute(in, 75) }

    (
      Timings(0, p1._1, p2._1),
      Solution(Long.box(p1._2), Long.box(p2._2))
    )
