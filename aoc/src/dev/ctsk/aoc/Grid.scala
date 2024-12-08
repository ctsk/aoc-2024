package dev.ctsk.aoc

case class Point(x: Int, y: Int):
  def +(o: Point): Point = Point(x + o.x, y + o.y)
  def -(o: Point): Point = Point(x - o.x, y - o.y)
  def *(n: Int): Point = Point(n * x, n * y)

enum Direction:
  case Up, Right, Down, Left
  def turnRight: Direction = Direction.fromOrdinal((ordinal + 1) % 4)

  def toPoint: Point =
    this match
      case Up    => Point(-1, 0)
      case Right => Point(0, 1)
      case Down  => Point(1, 0)
      case Left  => Point(0, -1)

  def apply(p: Point): Point = p + toPoint

  def applyN(p: Point, n: Int): Point = p + toPoint * n

  def flip: Direction =
    this match
      case Up    => Down
      case Right => Left
      case Down  => Up
      case Left  => Right

case class Pose(pos: Point, dir: Direction):
  def turnRight: Pose = Pose(pos, dir.turnRight)
  def step: Pose = Pose(dir(pos), dir)
  def step(n: Int): Pose = Pose(dir.applyN(pos, n), dir)

class Grid[A](val data: Array[Array[A]]):
  def height: Int = data.length

  def width: Int = data(0).length

  def apply(p: Point): Option[A] =
    if p.x < 0 | p.y < 0 | p.x >= height | p.y >= width
    then None
    else Some(data(p.x)(p.y))

  def update(p: Point, a: A): Unit = data(p.x)(p.y) = a

  def find(f: A => Boolean): IndexedSeq[Point] =
    for
      i <- data.indices
      j <- data(i).indices
      if f(data(i)(j))
    yield Point(i, j)

  def findFirst(f: A => Boolean): Option[Point] =
    data.zipWithIndex.flatMap { (row, x) =>
      row.zipWithIndex.collect { case (a, y) if f(a) => Point(x, y) }
    }.headOption

  def count(f: A => Boolean): Int = data.map(_.count(f)).sum

  def contains(p: Point): Boolean =
    p.x >= 0 && p.y >= 0 && p.x < height && p.y < width
