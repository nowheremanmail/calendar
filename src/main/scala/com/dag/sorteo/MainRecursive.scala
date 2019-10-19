package com.dag.sorteo

object MainRecursive {
  var M = 0

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
      val allRemainMatches = matches
        .filter(b => a._1 != b)
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
        //println(c);
        c.seeFixtures(M)
      }
      else {
        //println("no more matches, pull")
      }
    }
    else {
      println(s"Calculating day ${days.head}")
      val mm = Utils.shuffle(matches)
      val dd = Utils.shuffle(days)
      val day = dd.head
      calculateFixtures(mm.filter(c.rulesDaily(_, day)), c, day, 10, mm, dd.tail)
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
