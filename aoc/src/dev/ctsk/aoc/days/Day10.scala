package dev.ctsk.aoc.days

import dev.ctsk.aoc._
import dev.ctsk.aoc.Direction._

object Day10 extends Solver(10):
  type Input = Vector[(Point, Point)]

  def pre(input: os.ReadablePath): Input =
    val grid = Grid(os.read.lines(input).map(_.toArray).toArray)

    def step(seq: Input, height: Int): Input =
      (for
        (p, origin) <- seq
        a <- Seq(Up, Down, Left, Right)
        if grid(a(p)).exists(_.asDigit == height)
      yield (a(p), origin)).toVector

    val zero = grid.find(_ == '0').toVector
    (1 to 9).foldLeft(zero.zip(zero)) { case (acc, h) => step(acc, h) }

  def part1(input: Input): Int = input.distinct.size

  def part2(input: Input): Int = input.size

  def run(input: os.ReadablePath): (Timings, Solution) =
    val (pre_time, pre_input) = timed { pre(input) }
    val (p1_time, p1_solution) = timed { part1(pre_input) }
    val (p2_time, p2_solution) = timed { part2(pre_input) }

    (
      Timings(pre_time, p1_time, p2_time),
      Solution(Int.box(p1_solution), Int.box(p2_solution))
    )
