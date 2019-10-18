package com.dag.sorteo

class Calendar {


  val cal = Array.ofDim[Int](20, 20);
  var N = 0;

  /*
    for {
      i <- 0 until 20
      j <- 0 until 20
    } cal(i)(j) = 0;
  */
  def isFull(): Boolean = (N == 20 * 20 - 20);

  def isFinished(): Boolean = (N == (20 * 20 - 20) / 2);

  def IsAllFixtures(): Boolean = (N > 0 && N % 10 == 0);

  def newWith(d: Int, m: Match): Calendar = {
    val c = new Calendar;

    for {
      i <- 0 until 20
      j <- 0 until 20
    } {
      c.cal(i)(j) = cal(i)(j)
    };

    c.cal(m.home.code-1)(m.visitor.code-1) = d;
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
}

object Main {

  def calculateFixtures(fixturesMatches: List[Match], c: Calendar, day: Int, value: Int, matches: List[Match], days: List[Int]): Unit = {

    if (value == 0) {
      calculate(matches, c, days)
      return;
    }
    if (fixturesMatches.isEmpty) return;

    println (s"Calculating day ${day} step ${value}")

    fixturesMatches.foreach(a => {
      val possibleFixturesMatches = fixturesMatches.filter(b => b.compatibleOnFixture(a));

      calculateFixtures(possibleFixturesMatches, c.newWith(day, a), day, value - 1, matches.filter(b => a != b), days)
    });

  }

  def calculate(matches: List[Match], c: Calendar, days: List[Int]): Unit = {

    if (matches.isEmpty) {
      if (c.isFinished()) {
        println (c);
      }
      println ("no more matches")
      return;
    }

    println (s"Calculating day ${days.head}")
    calculateFixtures(matches, c, days.head, 10, matches, days.tail)

  }

  def main(args: Array[String]): Unit = {
    val teams = (1 to 20).map { i => new Team(i, s"team $i") }
    val days = (1 to 38).toList
    val matches = teams.flatMap(a => teams.filter(c => c != a).map(b => new Match(a, b))).toList

    val c = new Calendar;
    calculate(matches, c, days);

  }
}
