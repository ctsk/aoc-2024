package dev.ctsk.aoc.days

import dev.ctsk.aoc._
import scala.annotation.nowarn

object Day05 extends Solver(5):
  def pre(input: os.ReadablePath) =
    val Array(rulesStr, updatesStr) = os.read(input).split("\n\n")

    val rules = rulesStr.linesIterator
      .map(rule => {
        val Array(a, b) = rule.split("""\|""")
        (a.toInt, b.toInt)
      })
      .toSet

    val updates =
      updatesStr.linesIterator.map(_.split(",").map(_.toInt).toVector).toVector

    (rules, updates)

  def run(input: os.ReadablePath): (Timings, Solution) =
    val (pre_time, (rules, updates)) = timed { pre(input) }

    @nowarn("msg=match may not be exhaustive")
    def isOrdered(u: Vector[Int]): Boolean =
      !u.combinations(2).exists { case Seq(a, b) =>
        rules.contains((b, a))
      }

    def findMiddle(u: Vector[Int]): Int =
      u.find(e =>
        u.count(rules.contains(_, e)) <= u.length / 2
          && u.count(rules.contains(e, _)) <= u.length / 2
      ).get

    val p1 = timed { updates.filter(isOrdered).map(u => u(u.length / 2)).sum }
    val p2 = timed { updates.filterNot(isOrdered).map(findMiddle).sum }

    (Timings(pre_time, p1._1, p2._1), Solution(Int.box(p1._2), Int.box(p2._2)))
