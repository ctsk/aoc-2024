package dev.ctsk.aoc.days

import dev.ctsk.aoc._

object Day04 extends Solver(4):

  def part1(lines: Array[Array[Char]]): Int =
    def pad(front: Int, line: Array[Char], end: Int): Array[Char] =
      Array.fill(front)('.') ++ line ++ Array.fill(end)('.')

    def count(line: Array[Char]) =
      (0 until line.length - 3)
        .count(idx => {
          (line.slice(idx, idx + 4) sameElements Array('X', 'M', 'A', 'S'))
          || (line.slice(idx, idx + 4) sameElements Array('S', 'A', 'M', 'X'))
        })

    lines.map(count(_)).sum
      + lines.transpose.map(count(_)).sum
      + lines.zipWithIndex
        .map((line, index) => pad(index, line, lines.length - index))
        .transpose
        .map(count(_))
        .sum
      + lines.zipWithIndex
        .map((line, index) => pad(lines.length - index, line, index))
        .transpose
        .map(count(_))
        .sum

  def part2(grid: Array[Array[Char]]): Int =
    def countXMAS(g: Array[Array[Char]]): Int =
      (for
        i <- 0 until g.length - 2
        j <- 0 until g(i).length - 2
        if g(i)(j) == 'M' && g(i)(j + 2) == 'M'
          && g(i + 1)(j + 1) == 'A'
          && g(i + 2)(j) == 'S' && g(i + 2)(j + 2) == 'S'
      yield 1).sum

    List(
      grid,
      grid.reverse,
      grid.transpose,
      grid.transpose.reverse
    ).map(countXMAS).sum

  def run(input: String): (Timings, Solution) =
    var in = io.Source
      .fromFile(input)
      .getLines()
      .map { line => line.toArray }
      .toArray

    val (p1_time, p1_solution) = timed { part1(in) }
    val (p2_time, p2_solution) = timed { part2(in) }

    (
      Timings(0, p1_time, p2_time),
      Solution(Int.box(p1_solution), Int.box(p2_solution))
    )
