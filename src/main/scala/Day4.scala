// https://adventofcode.com/2018/day/4

import scala.collection.mutable.HashMap

object Day4 {
  // Match yields (month, day, hour, minute, status, Maybe Guard#)
  val regex = "\\[1518-([0-9]{2})-([0-9]{2}) ([0-9]{2}):([0-9]{2})\\] (wakes|falls|Guard #([0-9]+))".r.unanchored
  val input = scala.io.Source.fromFile("input4.txt").mkString.split('\n')


  // Generate a mapping of { Guard# -> minutes the guard slept}
  def guardStore = {
    val store = HashMap[Int, Seq[Int]]()
    var curGuard = 0
    var startMinute = 0
    
    input
    .map({ case regex(mon, day, hr, min, status, guard) => (mon, day, hr, min, status, guard) }) // regex parse
    .sorted  // sort tuples to get chronological order
    .foreach(_ match {
      case (_,_,_, min, "wakes", null) => {                           // Match case where the guard wakes up and we can calculate how many minutes he slept
        var cur = store.getOrElseUpdate(curGuard, Seq[Int]())
        store.put(curGuard, cur ++ (startMinute until min.toInt))
      }
      case (_,_,_, min, "falls", null) => startMinute = min.toInt     // Match case where guard falls asleep
      case (_,_,_,_,_, guard) => curGuard = guard.toInt               // Start the day for this guard
    })
    store
  }

  def solutionPartOne = {
    val store = guardStore

    // get the guard who slept the most minutes
    var maxGuard = store
      .map({ case (k,v) => (k, v.size) })
      .maxBy({ case (k,v) => v })

    // Get the minute the guard slept the most
    var minute = store
                  .getOrElse(maxGuard._1, List())
                  .groupBy(identity).mapValues(_.size)
                  .maxBy({case (k,v) => v})
    maxGuard._1 * minute._1
  }

  def solutionPartTwo = {
    val store = guardStore
    var result = store.map({
      // For each guard, find the minute each slept the most
      case (k,v) => (k, v.groupBy(identity).mapValues(_.size).maxBy(_._2))
    })
    // Find the guard who consistently slept the most at a particular minute
    .maxBy(_._2._2)
    result._1 * result._2._1
  }
}