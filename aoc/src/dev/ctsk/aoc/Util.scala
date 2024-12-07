package dev.ctsk.aoc

def NAT_REGEX = """(\d+)""".r

def longs(string: String): Vector[Long] =
  NAT_REGEX.findAllIn(string).map(_.toLong).toVector
