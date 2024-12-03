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

  pattern
    .replaceAllIn(
      memory,
      matched => matched.group(1) match {
        case null => " "
        case _ => matched.group(1)
      }
    )
    .split("""\s+""")
    .filterNot(_.isEmpty)
    .toList

def parseMul(instruction: String): (Int, Int) =
  val pattern: Regex = """mul\((\d{1,3}),(\d{1,3})\)""".r
  val pattern(a, b) = instruction
  (a.toInt, b.toInt)

def parseConditions(memory: List[String]): List[String] =
  val pattern = """(do\(\)|don't\(\))""".r
  var takeMuls = true
  var instructions = List[String]()

  for instruction <- memory do
    pattern.findFirstIn(instruction) match
      case Some("do()") => takeMuls = true
      case Some("don't()") => takeMuls = false
      case None => if takeMuls then instructions = instructions :+ instruction

  getValidInstructions(instructions.mkString(""))

def part1(memory: String): Int =
  val instructions = getValidInstructions(memory)
  val parsedInstructions = instructions
    .map(parseMul)
    .map((a, b) => a * b)
    .sum

  parsedInstructions

def part2(memory: String): Int =
  val instructions = getValidInstructionsExtended(memory)
  println(instructions)
  val parsedInstructions = parseConditions(instructions)
    .map(parseMul)
    .map((a, b) => a * b)
    .sum

  parsedInstructions

@main
def day3(): Unit =
  val memory = getFileAsString

  println(memory)
  //  println(part1(memory))
  // too low 83429015
  println(part2(memory))
