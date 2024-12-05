package dev.ctsk.aoc.days

import dev.ctsk.aoc._

object Day02 extends Solver(2):
  def pre(input: os.ReadablePath): List[List[Int]] =
    os.read
      .lines(input)
      .map(line => line.split(" ").map(_.toInt).toList)
      .toList

  def safe(list: List[Int]): Boolean =
    list.length < 2 || list
      .sliding(2)
      .collect { case List(a, b) => b - a }
      .forall(1 to 3 contains _)

  def safeWithGap(list: List[Int]): Boolean =
    list.indices.exists(i => safe(list.take(i) ++ list.drop(i + 1)))

  def part1(lists: List[List[Int]]): Int =
    lists.count(list => safe(list) || safe(list.reverse))

  def part2(lists: List[List[Int]]): Int =
    lists.count(list => safeWithGap(list) || safeWithGap(list.reverse))

  def run(input: os.ReadablePath): (Timings, Solution) =
    val (pre_time, pre_input) = timed { pre(input) }
    val (p1_time, p1_solution) = timed { part1(pre_input) }
    val (p2_time, p2_solution) = timed { part2(pre_input) }

    (
      Timings(pre_time, p1_time, p2_time),
      Solution(Int.box(p1_solution), Int.box(p2_solution))
    )
