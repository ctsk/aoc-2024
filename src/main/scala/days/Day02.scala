package dev.ctsk.aoc.days

import dev.ctsk.aoc._

object Day02 extends Solver(2):
  def pre(input: String): List[List[Int]] =
    io.Source
      .fromFile(input)
      .getLines()
      .map(line => line.split(" ").map(_.toInt).toList)
      .toList

  def valid(list: List[Int]): Boolean =
    val isSorted = (list.sorted == list || list.sorted.reverse == list)
    val notTooLarge = list.sliding(2).map(x => (x(1) - x(0)).abs).forall(_ <= 3)
    val notTooSmall = list.sliding(2).map(x => (x(1) - x(0)).abs).forall(_ >= 1)
    isSorted && notTooLarge && notTooSmall

  def part1(lists: List[List[Int]]): Int = lists.count(valid)

  def part2(lists: List[List[Int]]): Int =
    lists.count(list =>
      list.indices.exists(i => valid(list.take(i) ++ list.drop(i + 1)))
    )

  def run(input: String): (Timings, Solution) =

    val (pre_time, pre_input) = timed { pre(input) }
    val (p1_time, p1_solution) = timed { part1(pre_input) }
    val (p2_time, p2_solution) = timed { part2(pre_input) }

    (
      Timings(pre_time, p1_time, p2_time),
      Solution(Int.box(p1_solution), Int.box(p2_solution))
    )
