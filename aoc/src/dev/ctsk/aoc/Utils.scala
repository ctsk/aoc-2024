package dev.ctsk.aoc

def gcd(a: Int, b: Int): Int =
  if b == 0 then a else gcd(b, a % b)

extension [A](xs: Seq[A])
  def pairs: Iterator[(A, A)] =
    xs.combinations(2).map(xs => (xs(0), xs(1)))
