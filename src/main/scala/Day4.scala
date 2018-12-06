// https://adventofcode.com/2018/day/4

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Map

object Day4 {
  // Match yields (month, day, hour, minute, status, Maybe Guard#)
  val regex = "\\[1518-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\\] (wakes|falls|Guard #([0-9]+))".r.unanchored
  val input = scala.io.Source.fromFile("input4.txt").mkString.lines

  def solutionPartOne = {
    // A map to store our (Date : PriorityQueue)
    val store = Map[Tuple2[Int, Int], PriorityQueue[Tuple2[Int, String]]]()

    // Separate each day out into buckets, and sort within those days with a PriorityQueue
    input.map(_ match {
      case regex(month, day, hour, minute, status, null) => ((month.toInt, day.toInt), (minute.toInt, status))
      case regex(month, day, hour, minute, status, guard_num) => {
        // This fool started work early. He's not technicallly working till the next day. (I'm hoping no one does this at the end of the month)
        if(hour == "23")
          ((month.toInt, day.toInt+1), (0, guard_num))
        else
          ((month.toInt, day.toInt), (minute.toInt, guard_num))
      }
    })
    .foreach( {case (date, status) => 
      store.getOrElseUpdate(date, PriorityQueue[Tuple2[Int, String]]()).enqueue(status)
    })

//    store.toIterator.map()
    ""
  }
}