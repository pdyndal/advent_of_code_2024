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
  var numberOfErrorsIncreasing = 0
  var numberOfErrorsDecreasing = 0

  val isIncreasing = list
    .sliding(2)
    .forall {
      case List(a, b) => {
        if a < b then true
        else {
          numberOfErrorsIncreasing += 1
          numberOfErrorsIncreasing <= 1
        }
      }
    }

  val isDecreasing = list
    .sliding(2)
    .forall {
      case List(a, b) => {
        if a > b then true
        else {
          numberOfErrorsDecreasing += 1
          numberOfErrorsDecreasing <= 1
        }
      }
    }

  isIncreasing || isDecreasing


def checkDifference(list: List[Int]): Boolean =
  list
    .sliding(2)
    .forall { case List(a, b) => Math.abs(a - b) >= 1 && Math.abs(a - b) <= 3 }

def checkDifferenceWithBadLevel(list: List[Int]): Boolean =
  var numberOfErrors = 0

  list
    .sliding(2)
    .forall {
      case List(a, b) => {
        if Math.abs(a - b) >= 1 then true
        else if Math.abs(a - b) <= 3 then true
        else {
          numberOfErrors += 1
          numberOfErrors <= 1
        }
      }
    }

def getValidLists(): List[List[Int]] =
  getFileAsLists()
    .filter(checkListIncreasingOrDecreasing)
    .filter(checkDifference)

def getValidListsWithSingleBadElement(): List[List[Int]] =
  getFileAsLists()
    .filter(checkListIncreasingOrDecreasingWithBadLevel)
    .filter(checkDifference)

@main
def day2(): Unit =
  val part1 = getValidLists()
  val part2 = getValidListsWithSingleBadElement()

  println(part1.length)
  println(part2.length)