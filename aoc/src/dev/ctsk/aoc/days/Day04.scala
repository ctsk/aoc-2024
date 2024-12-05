package dev.ctsk.aoc.days

import dev.ctsk.aoc._
import scala.collection.mutable.ArrayBuffer

object Day04 extends Solver(4):
  private val XMAS = "XMAS".r
  private val REV_XMAS = "XMAS".reverse.r

  def diagonals[A](m: Vector[Vector[A]]): Vector[Vector[A]] =
    val diagonals = Vector.fill(m.length + m(0).length - 1)(ArrayBuffer[A]())

    for
      i <- m.indices
      j <- m(i).indices
    do diagonals(i + j) += m(i)(j)

    diagonals.map(_.toVector)

  def part1(lines: Vector[Vector[Char]]): Int =
    def count(line: Vector[Char]) =
      val cs = ArrayCharSequence(line.toArray)
      XMAS.findAllIn(cs).length + REV_XMAS.findAllIn(cs).length

    Vector(
      lines,
      lines.transpose,
      diagonals(lines),
      diagonals(lines.reverse)
    ).flatMap(_.map(count)).sum

  def part2(grid: Vector[Vector[Char]]): Int =
    (for
      i <- 1 until grid.length - 1
      j <- 1 until grid(i).length - 1
      if grid(i)(j) == 'A'
        && grid(i - 1)(j - 1) + grid(i + 1)(j + 1) == 'S' + 'M'
        && grid(i - 1)(j + 1) + grid(i + 1)(j - 1) == 'S' + 'M'
    yield ()).length

  def run(input: String): (Timings, Solution) =
    var in = io.Source
      .fromFile(input)
      .getLines()
      .map { line => line.toVector }
      .toVector

    val (p1_time, p1_solution) = timed { part1(in) }
    val (p2_time, p2_solution) = timed { part2(in) }

    (
      Timings(0, p1_time, p2_time),
      Solution(Int.box(p1_solution), Int.box(p2_solution))
    )
