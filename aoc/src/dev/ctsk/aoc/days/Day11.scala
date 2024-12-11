package dev.ctsk.aoc.days

import dev.ctsk.aoc._
import scala.collection.mutable.{Map => MuMap}

object Day11 extends Solver(11):
  private def halves(n: Long): Option[(Long, Long)] =
    val numDigits = (Math.log10(n) + 1).toLong
    if numDigits % 2 == 1 then None
    else
      val mod = Math.pow(10, numDigits / 2).toLong
      Some(n % mod, n / mod)

  private def count(initial: Seq[Long], depth: Int): Long =
    def step(stones: MuMap[Long, Long]): MuMap[Long, Long] =
      val next = MuMap.empty[Long, Long].withDefaultValue(0L)
      for ((stone, count) <- stones) {
        if stone == 0 then next(1) += count
        else
          halves(stone) match
            case Some((a, b)) =>
              next(a) += count
              next(b) += count
            case None =>
              next(stone * 2024) += count
      }
      next

    val initMap = MuMap.from(initial.map((_, 1L)))
    Seq.iterate(initMap, depth + 1)(step).last.values.sum

  def run(input: os.ReadablePath): (Timings, Solution) =
    val (pre_time, in) = timed { longs(os.read.lines(input).head) }
    val (p1_time, p1_solution) = timed { count(in, 25) }
    val (p2_time, p2_solution) = timed { count(in, 75) }

    (
      Timings(pre_time, p1_time, p2_time),
      Solution(Long.box(p1_solution), Long.box(p2_solution))
    )
