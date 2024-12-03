package dev.ctsk.aoc

import dev.ctsk.aoc.days._

val solvers = Map[Int, Solver](
  1 -> Day01,
  2 -> Day02,
  3 -> Day03
)


@main def main(day: String, input: String): Unit =
  solvers.get(day.toInt) match
    case Some(solver) =>
      val (timings, solution) = solver.run(input)
      sys.env.get("AOC_BENCH") match
        case Some(_) =>
          println(solution.p1)
          println(solution.p2)
        case None =>
          println(f"Preprocessing: ${timings.prep}%24s μs")
          println(f"Part 1: ${solution.p1}%15s ${timings.p1}%15s μs")
          println(f"Part 2: ${solution.p2}%15s ${timings.p2}%15s μs")

    case None =>
      println(s"Day $day not solved")
