// https://adventofcode.com/2018/day/3

object Day3 {
  val regex = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
  val input = scala.io.Source.fromFile("input3.txt").mkString.split('\n') 

  case class Rect(x: Int, y: Int, width: Int, height: Int)

  def rectToTup(rect: Rect): Seq[Tuple2[Int, Int]] = {
    for (x <- rect.x + 1 to rect.x + rect.width; y <- rect.y +1 to rect.y + rect.height)
      yield (x,y)
  }

  // SLOW AF, FIX
  def solutionPartOne = {
    input.map(_ match {
      case regex(id, x, y, w, h) => Rect(x.toInt, y.toInt, w.toInt, h.toInt)
    })
    .flatMap(rectToTup)
    .groupBy(identity)
    .mapValues(_.size)
    .values
    .count(_ > 1)
  }
}