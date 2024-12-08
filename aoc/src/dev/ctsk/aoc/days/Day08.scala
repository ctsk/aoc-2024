package dev.ctsk.aoc.days

import dev.ctsk.aoc._

extension [A](xs: Seq[A])
  def pairs: Iterator[(A, A)] =
    xs.combinations(2).map(xs => (xs(0), xs(1)))

object Day08 extends Solver(8):
  type Input = (Vector[Vector[Point]], Grid[Char])

  case class Ctx(f: os.ReadablePath):
    val grid = Grid(os.read.lines(f).map(_.toArray).toArray)
    val antennae =
      grid.find(_ != '.').groupBy(grid(_)).map(_._2.toVector).toVector

    def antinodes(a: Point, b: Point): Iterator[Point] =
      def it(a: Point, b: Point) =
        Iterator.iterate(a)(_ + (a - b).reduce).takeWhile(grid.contains)
      it(a, b) ++ it(b, a)

    def part1: Int =
      antennae
        .flatMap(
          _.pairs
            .flatMap((a, b) => Iterator(a + a - b, b + b - a))
            .filter(grid.contains)
        )
        .toSet
        .size

    def part2: Int =
      antennae
        .flatMap(_.pairs.flatMap(p => antinodes(p._1, p._2)))
        .toSet
        .size
  def run(input: os.ReadablePath): (Timings, Solution) =
    val (pre_time, ctx) = timed { Ctx(input) }
    val (p1_time, p1_solution) = timed { ctx.part1 }
    val (p2_time, p2_solution) = timed { ctx.part2 }

    (
      Timings(pre_time, p1_time, p2_time),
      Solution(Int.box(p1_solution), Int.box(p2_solution))
    )
