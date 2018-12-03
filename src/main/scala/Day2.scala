
// https://adventofcode.com/2018/day/2
object Day2 {
  val input = scala.io.Source.fromFile("input2.txt").mkString.split('\n')

  case class Counts(two: Boolean, three: Boolean)

  def countChars(str: String): Counts = {
    var two = false
    var three = false
    str.groupBy(identity)
      .mapValues(_.size)
      .values
      .foreach(v => {
        if (v == 2) two = true
        else if (v == 3) three = true
      })
    return Counts(two, three)
  }

  def solutionPartOne: Int = {
    var twos = 0
    var threes = 0
    input
      .foreach(str => {
        countChars(str) match {
          case Counts(true, false) => twos += 1
          case Counts(false, true) => threes += 1
          case Counts(true, true) => {
            twos += 1
            threes += 1
          }
          case Counts(false, false) => ""
        }
      })
    return twos * threes
  }

  def getDistance(str1: String, str2: String) = {
      str1.zip(str2).count(k=> k._1 != k._2)
  }

  def solutionPartTwo: String = {
      ""
  }

}