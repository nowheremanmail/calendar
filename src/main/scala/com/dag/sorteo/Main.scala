package com.dag.sorteo

class Calendar {
  val cal = Array.ofDim[Int](20, 20);

  for {
    i <- 0 until 20
    j <- 0 until 20
  } cal(i)(j) = 0;

  def newWith(d: Int, m: Match): Calendar = {
    val c = new Calendar;

    for {
      i <- 0 until 20
      j <- 0 until 20
    } c.cal(i)(j) = cal(i)(j);

    c.cal(m.home.code)(m.visitor.code) = d;
    return c;
  }

  def print(): Unit = {
    for {
      i <- 0 until 20
      j <- 0 until 20
    }
      if (i != j)
        println(s"($i)($j) = ${cal(i)(j)}")
  }
}

object Main {

  def calculate(matches: List[Match], days: List[Int], c: Calendar, count: Int): Unit = {
    // take 10

    if (count > 0 && count % 10 == 0) {
      if (days.length <= 1) {
        c.print();
        return;
      }

      calculate(matches, days.tail, c, count);
    }
    else {
      matches.foreach(a => {
        calculate(matches.filter(b => a != b), days, c.newWith(days.head, a), count + 1)
      });
    }
  }

  def main(args: Array[String]): Unit = {
    val teams = (0 until 20).map { i => new Team(i, i + "") }
    val days = (1 to 38).toList
    val matches = teams.flatMap(a => teams.filter(c => c != a).map(b => new Match(a, b))).toList

    val c = new Calendar;
    calculate(matches, days, c, 0);

  }
}
