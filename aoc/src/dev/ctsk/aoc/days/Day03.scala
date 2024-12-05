package dev.ctsk.aoc.days

import dev.ctsk.aoc._

object Day03 extends Solver(3):
  def part1(input: String): Int =
    val pattern = """mul\((\d{1,3}),(\d{1,3})\)""".r
    pattern
      .findAllIn(input)
      .matchData
      .map(m => m.group(1).toInt * m.group(2).toInt)
      .sum

  def part2(line: String): Int =
    val pattern = """(mul\(\d{1,3},\d{1,3}\))|do\(\)|don't\(\)""".r;
    pattern
      .findAllIn(line)
      .foldLeft((true, 0)) { case ((enabled, sum), curr) =>
        curr match
          case "do()"    => (true, sum)
          case "don't()" => (false, sum)
          case s"mul($a,$b)" =>
            (enabled, if enabled then sum + a.toInt * b.toInt else sum)
      }
      ._2

  def run(input: os.ReadablePath): (Timings, Solution) =
    val in = os.read(input)
    val (p1_time, p1_solution) = timed { part1(in) }
    val (p2_time, p2_solution) = timed { part2(in) }

    (
      Timings(0, p1_time, p2_time),
      Solution(Int.box(p1_solution), Int.box(p2_solution))
    )
