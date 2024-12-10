package dev.ctsk.aoc.days

import dev.ctsk.aoc._
import dev.ctsk.aoc.Direction._

object Day10 extends Solver(10):
  type Input = (Grid[Char], Seq[Point])

  def pre(input: os.ReadablePath): Input =
    val grid = Grid(os.read.lines(input).map(_.toArray).toArray)
    val zero = grid.find(_ == '0').toSeq
    (grid, zero)

  def step(
      grid: Grid[Char],
      start: Seq[(Point, Point)],
      height: Int
  ): Seq[(Point, Point)] =
    (for
      (p, origin) <- start
      a <- Seq(Up, Down, Left, Right)
      if grid(a(p)).exists(_.asDigit == height)
    yield (a(p), origin))

  def part1(input: Input): Int =
    val (grid, zero) = input
    (1 to 9)
      .foldLeft(zero.zip(zero)) { case (acc, h) => step(grid, acc, h) }
      .distinct
      .size

  def part2(input: Input): Int =
    val (grid, zero) = input
    (1 to 9)
      .foldLeft(zero.zip(zero)) { case (acc, h) => step(grid, acc, h) }
      .size

  def run(input: os.ReadablePath): (Timings, Solution) =
    val (pre_time, (grid, zero)) = timed { pre(input) }
    val (p1_time, p1_solution) = timed { part1((grid, zero)) }
    val (p2_time, p2_solution) = timed { part2((grid, zero)) }

    (
      Timings(pre_time, p1_time, p2_time),
      Solution(Int.box(p1_solution), Int.box(p2_solution))
    )
