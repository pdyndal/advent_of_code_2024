package day2

import scala.io.Source

def mapLineToList(line: String): List[Int] =
  line
    .split(" ")
    .filter(_.nonEmpty)
    .map(_.toInt)
    .toList

def getFileAsLists(): List[List[Int]] =
  Source
    .fromResource("day_2.txt")
    .getLines
    .toList
    .map(mapLineToList)

def checkListIncreasingOrDecreasing(list: List[Int]): Boolean =
  val isIncreasing = list
    .sliding(2)
    .forall { case List(a, b) => a < b }

  val isDecreasing = list
    .sliding(2)
    .forall { case List(a, b) => a > b }

  isIncreasing || isDecreasing

def checkListIncreasingOrDecreasingWithBadLevel(list: List[Int]): Boolean =
  def isValidIncreasing(list: List[Int]): Boolean =
    list.sliding(2).forall { case List(a, b) => a < b }

  def isValidDecreasing(list: List[Int]): Boolean =
    list.sliding(2).forall { case List(a, b) => a > b }

  var isIncreasing = isValidIncreasing(list)
  var isDecreasing = isValidDecreasing(list)

  if isIncreasing || isDecreasing then true
  else
    list.indices.exists { i =>
      isIncreasing = isValidIncreasing(list.take(i) ++ list.drop(i + 1))
      isDecreasing = isValidDecreasing(list.take(i) ++ list.drop(i + 1))
      isIncreasing || isDecreasing
    }

def checkDifference(list: List[Int]): Boolean =
  list
    .sliding(2)
    .forall { case List(a, b) => Math.abs(a - b) >= 1 && Math.abs(a - b) <= 3 }

def checkDifferenceWithBadLevel(list: List[Int]): Boolean =
  def isValid(lst: List[Int]): Boolean =
    lst.sliding(2).forall { case List(a, b) => Math.abs(a - b) >= 1 && Math.abs(a - b) <= 3 }

  if isValid(list) then true
  else
    list.indices.exists { i => isValid(list.take(i) ++ list.drop(i + 1)) }


def getValidLists(): List[List[Int]] =
  getFileAsLists()
    .filter(checkListIncreasingOrDecreasing)
    .filter(checkDifference)

def getValidListsWithSingleBadElement(): List[List[Int]] =
  getFileAsLists()
    .filter(checkListIncreasingOrDecreasingWithBadLevel)
    .filter(checkDifferenceWithBadLevel)

@main
def day2(): Unit =
  val part1 = getValidLists()
  val part2 = getValidListsWithSingleBadElement()

  println(part1.length)
  println(part2.length)