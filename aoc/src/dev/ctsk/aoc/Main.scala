package dev.ctsk.aoc

import dev.ctsk.aoc.days._

val solvers = Map[Int, Solver](
  1 -> Day01,
  2 -> Day02,
  3 -> Day03,
  4 -> Day04,
  5 -> Day05
)

def runSolver(solver: Solver, input: os.Path): Unit =
  val (timings, solution) = solver.run(input)
  sys.env.get("AOC_BENCH") match
    case Some(_) =>
      println(solution.p1)
      println(solution.p2)
    case None =>
      println(f"Preprocessing: ${timings.prep}%24s μs")
      println(f"Part 1: ${solution.p1}%15s ${timings.p1}%15s μs")
      println(f"Part 2: ${solution.p2}%15s ${timings.p2}%15s μs")

@main def main(day: String, input: String): Unit =
  val num = """(\d+)""".r
  day match
    case "all" =>
      solvers.foreach { case (day, solver) =>
        runSolver(solver, os.Path(input, os.pwd) / f"$day%02d.in")
      }
    case num(_) =>
      solvers.get(day.toInt) match
        case Some(solver) => runSolver(solver, os.Path(input, os.pwd))
        case None         => println(s"Day $day not solved")
    case _ => println(day)
