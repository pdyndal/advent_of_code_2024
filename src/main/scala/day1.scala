import scala.io.Source

def mapLineToTuple(line: String): (Int, Int) =
  val split = line
    .split(" ")
    .filter(_.nonEmpty)

  (split(0).toInt, split(1).toInt)

def getFileAsTuple: (List[Int], List[Int]) =
  var leftList = List.empty[Int]
  var rightList = List.empty[Int]

  Source
    .fromResource("day_1.txt")
    .getLines
    .toList
    .map(mapLineToTuple)
    .foreach { case (left, right) =>
      leftList = leftList :+ left
      rightList = rightList :+ right
    }

  (leftList, rightList)

def getListsDistance: Int =
  val (leftList, rightList) = getFileAsTuple

  leftList
    .sorted
    .zip(rightList.sorted)
    .map { case (left, right) =>
      Math.abs(left - right)
    }
    .sum
  
def getSimilarityScore: Int =
  val (leftList, rightList) = getFileAsTuple

  leftList
    .map( item => {
      item * rightList.count(_ == item)
    })
    .sum

@main
def day1(): Unit =
  println(getListsDistance)
  println(getSimilarityScore)