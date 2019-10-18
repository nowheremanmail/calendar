package com.dag.sorteo

class Calendar(val teams: List[Team]) {
  val cal = Array.ofDim[Int](20, 20);
  var N = 0;

  /*
    for {
      i <- 0 until 20
      j <- 0 until 20
    } cal(i)(j) = 0;

  def isFinished: Boolean = (N == (20 * 20 - 20) / 2);

  def IsAllFixtures(): Boolean = (N > 0 && N % 10 == 0);
  */
  def isFull: Boolean = (N == 20 * 20 - 20);

  def newWith(d: Int, m: Match): Calendar = {
    val c = new Calendar(teams);

    for {
      i <- 0 until 20
      j <- 0 until 20
    } {
      c.cal(i)(j) = cal(i)(j)
    };

    c.cal(m.home.code - 1)(m.visitor.code - 1) = d;
    c.N = N + 1;
    return c;
  }

  override def toString(): String = {
    var res = ""
    for {
      i <- 0 until 20
    } {
      for {
        j <- 0 until 20
      } {
        if (i != j) {
          val cel = cal(i)(j)
          res += f" $cel%2d "
        }
        else {
          res += "    "
        }
      }
      res += "\n"
    }
    res += "\n"
    res
  }

  def checkCalendar() = {

    var T = 0
    var fixtures = Array.ofDim[Set[Int]](38)
    for {
      i <- 0 until 38} {
      fixtures(i) = Set()
    }

    for {
      i <- 0 until 20
      j <- 0 until 20
    } {
      if (i != j) {
        T += 1
        fixtures(cal(i)(j) - 1) = fixtures(cal(i)(j) - 1) + (i + 1)
        fixtures(cal(i)(j) - 1) = fixtures(cal(i)(j) - 1) + (j + 1)
      }
    }

    if (T != N) throw new RuntimeException("Invalid N")

    for {
      i <- 0 until 38} {
      if (fixtures(i).size != 20) throw new RuntimeException("Invalid fixture " + (i + 1))
    }
  }

  def seeFixtures() = {

    var fixtures = Array.ofDim[Set[String]](38)
    for {
      i <- 0 until 38} {
      fixtures(i) = Set()
    }

    for {
      i <- 0 until 20
      j <- 0 until 20
    } {
      if (i != j) {
        fixtures(cal(i)(j) - 1) = fixtures(cal(i)(j) - 1) + f"${teams(i).name} - ${teams(j).name}"
      }
    }
    for {
      i <- 0 until 38} {
      println(i + " " + fixtures(i))
    }
  }

}

object Main {


  def calculateFixtures(fixturesMatches: List[Match], c: Calendar, day: Int, value: Int, matches: List[Match], days: List[Int]): Unit = {

    if (value == 0) {
      //println(s"Calculating day ${day} step ${value} , next")
      calculate(matches, c, days)
      return;
    }

    if (fixturesMatches.isEmpty || fixturesMatches.length < value || distinctTeams(fixturesMatches) < value * 2) {
      //println(s"Calculating day ${day} step ${value} , break")
      return
    };

    //println(s"Calculating day ${day} step ${value}")
    fixturesMatches.foreach(a => {
      val possibleFixturesMatches = fixturesMatches.filter(b => b.compatibleOnFixture(a))
        .filter(rules(a, day))

      calculateFixtures(possibleFixturesMatches, c.newWith(day, a), day, value - 1, matches.filter(b => a != b), days)

    });
  }

  def rules(a: Match, day: Int) = (b: Match) => {

    (a.home.name, b.home.name) match {
      case ("RMA", "ATM") => Set(1, 2, 3, 18).contains(day)
      case ("FCB", "ESP") => Set(1, 2, 3, 18).contains(day)
      case ("VAL", "LEV") => Set(1, 2, 3, 18, 19, 9).contains(day)
      case _ => true
    }

  }

  def distinctTeams(matches: List[Match]) = matches.flatMap(a => Set(a.home.code, a.visitor.code)).length

  def calculate(matches: List[Match], c: Calendar, days: List[Int]): Unit = {

    if (matches.isEmpty) {
      if (c.isFull) {
        c.checkCalendar()
        //println(c);
        c.seeFixtures()
      }
      else {
        //println("no more matches, pull")
      }
      return;
    }

    println(s"Calculating day ${days.head}")

    calculateFixtures(matches, c, days.head, 10, matches, days.tail)
  }

  def main(args: Array[String]): Unit = {
    val teams = List("ALA", "ATH", "ATM", "BAR", "CEL", "EIB", "ESP", "GET", "GRA", "LEG", "LEV", "MAL", "OSA", "BET", "RMA", "RSO", "SEV", "VAL", "VLL", "VIL")
      .zipWithIndex.map { case (a, i) => new Team(i + 1, a) }
    val days = (1 to 38).toList
    val matches = teams.flatMap(a => teams.filter(c => c != a).map(b => new Match(a, b))).toList

    calculate(scala.util.Random.shuffle(matches), new Calendar(teams), days);
  }


}
