import scala.io.Source
import scala.util.matching.Regex

def getFileAsString: String =
  Source
    .fromResource("day_3.txt")
    .getLines
    .mkString("")

def getValidInstructions(memory: String): List[String] =
  val mulPattern: Regex = """mul\(\d{1,3},\d{1,3}\)""".r

  mulPattern
    .findAllIn(memory)
    .toList

def getValidInstructionsExtended(memory: String): List[String] =
  val pattern = """(mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\))|.""".r
  val mulPattern: Regex = """mul\(\d{1,3},\d{1,3}\)""".r

  pattern
    .findAllIn(memory)
    .filter {
      case "do()" => true
      case "don't()" => true
      case mulPattern() => true
      case _ => false
    }
    .toList

def parseMul(instruction: String): (Int, Int) =
  val pattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r
  instruction match {
    case pattern(a, b) => (a.toInt, b.toInt)
    case _ => (0, 0)
  }

def parseConditions(memory: List[String]): List[String] =
  memory
    .foldLeft((true, List[String]())) {
      case ((takeMuls, instructions), instruction) =>
        instruction match {
          case "don't()" => (false, instructions)
          case "do()" => (true, instructions)
          case _ if takeMuls => (takeMuls, instructions :+ instruction)
          case _ => (takeMuls, instructions)
        }
    }
    ._2

def part1(memory: String): Int =
  getValidInstructions(memory)
    .map(parseMul)
    .map((a, b) => a * b)
    .sum

def part2(memory: String): Int =
  parseConditions(getValidInstructionsExtended(memory))
    .map(parseMul)
    .map((a, b) => a * b)
    .sum

@main
def day3(): Unit =
  val memory = getFileAsString

  println(part1(memory))
  println(part2(memory))
