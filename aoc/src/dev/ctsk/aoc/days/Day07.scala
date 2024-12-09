package dev.ctsk.aoc.days

import dev.ctsk.aoc._

object Day07 extends Solver(7):
  def concat(a: Long, b: Long): Long =
    var m: Long = 1
    while m * 10 <= b do m *= 10
    a * (m * 10) + b

  def deconcat(a: Long, b: Long): Option[Long] =
    val b_len = Math.log10(b).floor.toLong + 1
    if (a % Math.pow(10, b_len).toLong == b)
    then Some(a / Math.pow(10, b_len).toLong)
    else None

  class Searcher(target: Long, nums: Vector[Long], elephants: Boolean):
    def search(acc: Long = nums(0), pos: Int = 1): Boolean =
      if (pos >= nums.length) return acc == target
      search(acc + nums(pos), pos + 1)
      || search(acc * nums(pos), pos + 1)
      || (elephants && search(concat(acc, nums(pos)), pos + 1))

  class Tester(nums: Vector[Long], elephants: Boolean):
    def test(target: Long, pos: Int = nums.length - 1): Boolean =
      if pos == 0 then return target == nums(0)
      if target <= 0 then return false
      test(target - nums(pos), pos - 1)
      || (target % nums(pos) == 0 && test(target / nums(pos), pos - 1))
      || (elephants && deconcat(target, nums(pos)).exists(test(_, pos - 1)))

  def check(target: Long, nums: Vector[Long], thirdElephant: Boolean): Boolean =
    if nums.contains(0)
    then Searcher(target, nums, thirdElephant).search()
    else Tester(nums, thirdElephant).test(target)

  def run(input: os.ReadablePath): (Timings, Solution) =
    val REGEX = """(\d+): (.*)""".r
    val (pre_time, in) = timed {
      os.read
        .lines(input)
        .map { case REGEX(value, rest) => (value.toLong, longs(rest)) }
        .toVector
    }

    val (p1_time, p1_solution) = timed {
      in.filter(p => check(p._1, p._2, false)).map(_.head).sum
    }

    val (p2_time, p2_solution) = timed {
      in.filter(p => check(p._1, p._2, true)).map(_.head).sum
    }

    (
      Timings(pre_time, p1_time, p2_time),
      Solution(Long.box(p1_solution), Long.box(p2_solution))
    )
