package dev.ctsk.aoc.days

import dev.ctsk.aoc._

object Day01 extends Solver(1):
  type Input = (Array[Int], Array[Int])

  def pre(input: String): Input =
    io.Source
      .fromFile(input)
      .getLines()
      .map { case s"$i   $j" => (i.toInt, j.toInt) }
      .toArray
      .unzip match { case (left, right) => (left.sorted, right.sorted) }

  def part1(input: Input): String =
    input match
      case (left, right) => left.zip(right).map(_ - _).map(_.abs).sum.toString

  def part2(input: Input): String =
    val (left, right) = input;
    val counts = right.groupBy(identity).mapValues(_.length).toMap
    left.map(n => n * counts.getOrElse(n, 0)).sum.toString

  def run(input: String): (Timings, Solution) =
    val (pre_time, pre_input) = timed { pre(input) }
    val (p1_time, p1_solution) = timed { part1(pre_input) }
    val (p2_time, p2_solution) = timed { part2(pre_input) }

    (Timings(pre_time, p1_time, p2_time), Solution(p1_solution, p2_solution))
