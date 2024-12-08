package dev.ctsk.aoc.days

import dev.ctsk.aoc._
import dev.ctsk.aoc.Direction._
import scala.annotation.tailrec
import scala.collection.mutable

object Day06 extends Solver(6):
  private def analyse(grid: Grid[Char]): Array[Array[Array[Int]]] =
    val skipMap = Array.fill(grid.height, grid.width, 4)(-1)
    for
      obstacle <- grid.find(_ == '#')
      d: Direction <- Seq(Up, Down, Right, Left)
      (pt, dist) <- Iterator
        .iterate(d(obstacle))(d(_))
        .takeWhile(pt => grid(pt).exists(_ != '#'))
        .zipWithIndex
    do skipMap(pt.x)(pt.y)(d.flip.ordinal) = dist
    skipMap

  def run(input: os.ReadablePath): (Timings, Solution) =
    val grid = Grid(os.read.lines(input).map(_.toArray).toArray)
    val (pre_time, start) = timed { grid.findFirst(_ == '^').get }

    @tailrec def trace(start: Pose, acc: Set[Point] = Set(start)): Set[Point] =
      val next = start.step
      grid(next.pos) match
        case Some('#') => trace(start.turnRight, acc)
        case Some(_)   => trace(next, acc + next.pos)
        case None      => acc

    val (p1_time, guardRoute) = timed { trace(Pose(pos = start, dir = Up)) }
    val p1_solution = guardRoute.size

    val (p2_time, p2_solution) =
      timed {
        val skipMap = analyse(grid)

        def loops(start: Pose, obstacle: Point): Boolean =
          val seen = mutable.Set(start)
          @tailrec def rec(cur: Pose): Boolean =
            val next = cur.step
            grid(next.pos) match
              case Some('#') =>
                if seen.add(cur) then rec(cur.turnRight) else true
              case Some(_) =>
                if next.pos == obstacle then return rec(cur.turnRight)
                if next.pos.x == obstacle.x || next.pos.y == obstacle.y
                then rec(next)
                else
                  val steps = skipMap(cur.pos.x)(cur.pos.y)(cur.dir.ordinal)
                  steps != -1 && rec(cur.step(steps))
              case None => false
          rec(start)

        guardRoute.filter(_ != start).count(loops(Pose(start, Up), _))
      }

    (
      Timings(pre_time, p1_time, p2_time),
      Solution(Int.box(p1_solution), Int.box(p2_solution))
    )
