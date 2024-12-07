package dev.ctsk.aoc.days

import dev.ctsk.aoc._

object Day07 extends Solver(7):
  def concat(a: Long, b: Long): Long =
    var m: Long = 1
    while m * 10 <= b do m *= 10
    a * (m * 10) + b

  class Searcher(target: Long, nums: Vector[Long], elephants: Boolean):
    def search(acc: Long = nums(0), pos: Int = 1): Boolean =
      if (pos >= nums.length) return acc == target
      if (acc > target) return false
      search(acc + nums(pos), pos + 1)
        || search(acc * nums(pos), pos + 1)
        || (elephants && search(concat(acc, nums(pos)), pos + 1))
  def run(input: os.ReadablePath): (Timings, Solution) =
    val REGEX = """(\d+): (.*)""".r
    val (pre_time, in) = timed {
      os.read.lines(input).map {
        case REGEX(value, rest) => (value.toLong, longs(rest))
      }.toVector
    }

    val (p1_time, p1_solution) = timed {
      in
        .filter((target, nums) => Searcher(target, nums, false).search())
        .map(_.head)
        .sum
    }

    val (p2_time, p2_solution) = timed {
      in
        .filter((target, nums) => Searcher(target, nums, true).search())
        .map(_.head)
        .sum
    }

    (
      Timings(pre_time, p1_time, p2_time),
      Solution(Long.box(p1_solution), Long.box(p2_solution))
    )
