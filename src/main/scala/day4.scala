import scala.io.Source

type Point = (Int, Int)

enum Direction(val x: Int, val y: Int):
  case North extends Direction(-1, 0)
  case East extends Direction(0, 1)
  case South extends Direction(1, 0)
  case West extends Direction(0, -1)
  case NorthEast extends Direction(-1, 1)
  case SouthEast extends Direction(1, 1)
  case SouthWest extends Direction(1, -1)
  case NorthWest extends Direction(-1, -1)

def getFileAsList: List[List[Char]] =
  Source
    .fromResource("day_4.txt")
    .getLines
    .toList
    .map(_
      .toCharArray
      .toList
    )

def checkDirectionForWord(list: List[List[Char]], point: Point, direction: Direction): Boolean =
  val word = "XMAS"
  val (x, y) = point
  val (dx, dy) = (direction.x, direction.y)
  val wordLength = word.length
  var spelledWord = ""

  if list(x)(y) != word(0) then
    return false

  for i <- 0 until wordLength do
    val (xCurrent, yCurrent) = (x + dx * i, y + dy * i)
    if xCurrent >= 0 && xCurrent < list.length && yCurrent >= 0 && yCurrent < list(xCurrent).length then
      spelledWord += list(xCurrent)(yCurrent)

  spelledWord == word

def part1(list: List[List[Char]]): Int =
  val directions = Direction.values
  val points = for
    x <- list.indices
    y <- list(x).indices
  yield (x, y)

  var count = 0

  for point <- points do
    for direction <- directions do
      if checkDirectionForWord(list, point, direction) then
        count += 1

  count

@main
def day4(): Unit =
  val list = getFileAsList
  println(part1(list))