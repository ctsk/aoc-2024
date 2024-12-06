package dev.ctsk.aoc.days

import dev.ctsk.aoc._
import scala.compiletime.ops.boolean
import scala.util.boundary.break
import scala.compiletime.ops.double

object Day06 extends Solver(6):

  def patrol(
      grid: Array[Array[Char]],
      start: (Int, Int)
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
      val next = (pos._1 + dirs(dir)._1, pos._2 + dirs(dir)._2)
      if next._1 < 0 || next._1 >= grid.length
        || next._2 < 0 || next._2 >= grid(0).length
      then return (false, visited)
      else if grid(next._1)(next._2) == '#'
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

    val visited = patrol(lines, start)
    val p1 = visited._2().size
    var p2 = 0
    val (p2_time, _) = timed {
      for (i, j) <- visited._2()
      do
        lines(i)(j) = '#'
        if patrol(lines, start)._1 then p2 += 1
        lines(i)(j) = '.'
    }

    return (Timings(0, 0, p2_time), Solution(Int.box(p1), Int.box(p2)))
