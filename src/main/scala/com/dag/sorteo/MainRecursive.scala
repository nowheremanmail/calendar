package com.dag.sorteo

import java.io.{File, FileOutputStream, PrintWriter}

object MainRecursive {
  var M = 0
  val writer = new PrintWriter(new FileOutputStream(new File("calendars.txt" ), true))

  def calculateFixtures(fixturesMatches: List[Match], c: Calendar, day: Int, value: Int, matches: List[Match], days: List[Int]): Unit = {
    if (value == 0) {
      //println(s"Calculating day ${day} step ${value} , next")
      calculate(matches, c, days)
      return
    }

    if (fixturesMatches.isEmpty || fixturesMatches.length < value || distinctTeams(fixturesMatches) < value * 2) {
      //println(s"Calculating day ${day} step ${value} , break")
      return
    }

    //println(s"Calculating day ${day} step ${value}")
    val mm = fixturesMatches.zipWithIndex
    mm.foreach(a => {
      val possibleFixturesMatches = mm
        .filter(b => b._2 > a._2).map(b => b._1)
        .filter(b => b.compatibleOnFixture(a._1))
        .filter(c.rules(a._1, day))
      val allRemainMatches = matches.filter(b => a._1 != b)
      val cc = c.newWith(day, a._1)

      calculateFixtures(possibleFixturesMatches, cc, day, value - 1, allRemainMatches, days)
    })

  }

  def distinctTeams(matches: List[Match]) = matches.flatMap(a => Set(a.home.code, a.visitor.code)).length

  def calculate(matches: List[Match], c: Calendar, days: List[Int]): Unit = {
    if (matches.isEmpty) {
      if (c.isFull) {
        M = M + 1

        c.checkCalendar()
        val fixtures = c.getFixtures
        val msg = JsonUtil.toJson(fixtures)
        println(">>>>" + M + " " + msg);
        if (Cache.previous.contains(msg)) {
          throw new RuntimeException("REPEAT!")
        }
        else {
          Cache.previous.add(msg)
        }
        writer.append(msg)
        writer.append("\n")
        writer.flush()
      }
      else {
        //println("no more matches, pull")
      }
    }
    else {
      val mm = Utils.shuffle(matches)
      // order days in order of number of possible options
      // we will start with days with less possible matches
      val sortedDaysByComplexity = Utils.shuffle(days).map(day => (day , mm.filter(c.rulesDaily(_, day)))).sortBy(a=>a._2.size)
      val dayData = sortedDaysByComplexity.head
      println(s"Calculating day ${dayData._1}")
      calculateFixtures(dayData._2, c, dayData._1, 10, mm, sortedDaysByComplexity.tail.map(a=>a._1))
    }
  }

  def main(args: Array[String]): Unit = {
    val c = new Calendar
    val teams = c.teams.zipWithIndex.map { case (a, i) => new Team(i + 1, a) }
    val days = (1 to 38).toList
    val matches = teams.flatMap(a => teams.filter(c => c != a).map(b => new Match(a, b))).toList

    calculate(matches, c, days)
  }
}
