package dev.ctsk.aoc

case class Timings(prep: Long, p1: Long, p2: Long)

case class Solution(p1: Object, p2: Object)

abstract class Solver(day: Int):
  def run(input: os.Path): (Timings, Solution)

  def timed[A](solution: => A): (Long, A) =
    val start = System.nanoTime()
    val result = solution
    val end = System.nanoTime()

    ((end - start) / 1000, result)
