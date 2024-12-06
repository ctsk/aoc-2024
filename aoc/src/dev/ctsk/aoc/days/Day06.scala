package dev.ctsk.aoc.days

import dev.ctsk.aoc._
import scala.compiletime.ops.boolean
import scala.util.boundary.break
import scala.compiletime.ops.double
import scala.collection.parallel.CollectionConverters._

object Day06 extends Solver(6):

  def patrol(
      grid: Array[Array[Char]],
      start: (Int, Int),
      obstacle: (Int, Int)
  ): (Boolean, () => Vector[(Int, Int)]) =
    var seen =
      Array.fill(4)(Array.fill(grid.length)(Array.fill(grid(0).length)(false)))

    def visited() =
      (for
        row <- 0 until grid.length
        col <- 0 until grid(0).length
        if (0 until 4).exists(v => seen(v)(row)(col))
      yield (row, col)).toVector

    var dirs = Vector((-1, 0), (0, 1), (1, 0), (0, -1))
    var dir = 0
    var pos = start
    seen(dir)(pos._1)(pos._2) = true

    while true do
      var next = (pos._1 + dirs(dir)._1, pos._2 + dirs(dir)._2)
      if next._1 < 0 || next._1 >= grid.length
        || next._2 < 0 || next._2 >= grid(0).length
      then return (false, visited)
      else if grid(next._1)(next._2) == '#' || next == obstacle
      then dir = (dir + 1) % 4
      else if seen(dir)(next._1)(next._2)
      then return (true, visited)
      else
        seen(dir)(next._1)(next._2) = true
        pos = next

    (false, visited)

  def run(input: os.ReadablePath): (Timings, Solution) =
    val lines = os.read.lines(input).map(_.toArray).toArray

    var start =
      (for
        i <- 0 until lines.length
        j <- 0 until lines(i).length
        if lines(i)(j) == '^'
      yield (i, j)).head

    val (p1_time, visited) = timed { patrol(lines, start, (-1, -1))._2() }
    val p1_solution = visited.length

    val (p2_time, p2_solution) =
      timed {
        visited.par.count((i, j) => patrol(lines, start, (i, j))._1)
      }

    return (
      Timings(0, p1_time, p2_time),
      Solution(Int.box(p1_solution), Int.box(p2_solution))
    )
