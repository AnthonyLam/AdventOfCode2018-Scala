// https://adventofcode.com/2018/day/1
object Day1 {
  val pairs = "([+-])([0-9]+)".r
  val input = scala.io.Source.fromFile("input.txt").mkString.split('\n')

  val matcher = (res: Int, line: String) => {
      line match {
        case pairs("+", value) => res + value.toInt
        case pairs("-", value) => res - value.toInt
        case _ => res
      }
  }

  def solutionPartOne = {
    input.foldLeft(0)(matcher)
  }

  def solutionPartTwo = {
    var seen = new scala.collection.mutable.HashSet[Int]
    Iterator.continually(input).flatten
      .scanLeft(0)(matcher)
      .filter(v => {
        if (seen.contains(v)) {
          true
        } else {
          seen += v
          false
        }
      }).next
  }

}