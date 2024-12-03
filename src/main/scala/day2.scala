import scala.io.Source

def mapLineToList(line: String): List[Int] =
  line
    .split(" ")
    .filter(_.nonEmpty)
    .map(_.toInt)
    .toList

def getFileAsLists: List[List[Int]] =
  Source
    .fromResource("day_2.txt")
    .getLines
    .toList
    .map(mapLineToList)

def deltaInRange(delta: Int): Boolean =
  Math.abs(delta) >= 1 && Math.abs(delta) <= 3

def deltaSignMatches(prevDelta: Int, currentDelta: Int): Boolean =
  prevDelta.sign == currentDelta.sign

def isReportSafe(report: List[Int]): Boolean =
  report
    .sliding(2)
    .map { case List(a, b) => b - a }
    .toList
    .sliding(2)
    .forall { case List(a, b) => deltaInRange(a) && deltaInRange(b) && deltaSignMatches(a, b) }

def countSafeReports(reports: List[List[Int]]): Int =
  reports
    .count(isReportSafe)

def isReportSafeWithDampener(list: List[Int]): Boolean =
  if isReportSafe(list) then true
  else list
    .indices
    .exists { i =>
      val newReport = list.take(i) ++ list.drop(i + 1)
      isReportSafe(newReport)
    }

def countSafeReportsWithDampener(reports: List[List[Int]]): Int =
  reports
    .count(isReportSafeWithDampener)

@main
def day2(): Unit =
  val reports = getFileAsLists
  val safeReports = countSafeReports(reports)
  val safeReportsWithDampener = countSafeReportsWithDampener(reports)
  println(s"Safe reports: $safeReports")
  println(s"Safe reports with dampener: $safeReportsWithDampener")
