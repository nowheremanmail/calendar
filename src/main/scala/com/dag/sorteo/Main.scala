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

    c.cal(m.home.code)(m.visitor.code) = d;
    c.N = N + 1;
    return c;
  }

  def print(): Unit = {
    for {
      i <- 0 until 20
    } {
      for {
        j <- 0 until 20
      } {
        if (i != j) {
          printf(" %2d ", cal(i)(j))
        }
        else {
          printf("    ")
        }
      }
      println()
    }
    println()
  }
}

object Main {

  def calculateFixtures(fixturesMatches: List[Match], c: Calendar, day: Int, value: Int, matches: List[Match], days: List[Int]): Unit = {

    if (value == 0) {
      calculate(matches, c, days)
      return;
    }
    if (fixturesMatches.isEmpty )return;

    fixturesMatches.foreach(a => {
      calculateFixtures(fixturesMatches
        .filter(b => a != b)
        .filter(b => b.visitor != a.home && b.home != a.home )
        , c.newWith(day, a), day, value-1, matches.filter(b => a != b), days)
    });

  }

  def calculate(matches: List[Match], c: Calendar, days: List[Int]): Unit = {
    // take 10

    if (matches.isEmpty) {
      if (c.isFinished()) {
        c.print();
      }
      return;
    }

    calculateFixtures(matches, c, days.head, 10, matches, days.tail)

  }

  def main(args: Array[String]): Unit = {
    val teams = (0 until 20).map { i => new Team(i, i + "") }
    val days = (1 to 38).toList
    val matches = teams.flatMap(a => teams.filter(c => c != a).map(b => new Match(a, b))).toList

    val c = new Calendar;
    calculate(matches, c, days);

  }
}
