package com.dag.sorteo

import java.io.{File, PrintWriter}

class Calendar {
  val teams = List("ALA", "ATH", "ATM", "BAR", "CEL", "EIB", "ESP", "GET", "GRA", "LEG", "LEV", "MAL", "OSA", "BET", "RMA", "RSO", "SEV", "VAL", "VLL", "VIL")
  val cal = Array.ofDim[Int](20, 20)
  var N = 0

  def isFull: Boolean = (N == 20 * 20 - 20)

  def newWith(d: Int, m: Match): Calendar = {
    val c = new Calendar;

    for {
      i <- 0 until 20
      j <- 0 until 20
    } {
      c.cal(i)(j) = cal(i)(j)
    };

    c.cal(m.home.code - 1)(m.visitor.code - 1) = d
    c.N = N + 1
    //println(s"set - ${m.home} - ${m.visitor} = ${d}")
    c
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


  def getFixtures = {

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
        fixtures(cal(i)(j) - 1) = fixtures(cal(i)(j) - 1) + f"${teams(i)}-${teams(j)}"
      }
    }

    fixtures
  }

  val UCL = Set("BAR", "ATM", "RMA", "VAL")
  val UEL = Set("SEV", "GET", "ESP")

  def rulesDaily(a: Match, day: Int) = {
  // TODO not more than 2 days as local ???
    val previousMatchDay = cal (a.visitor.code-1)(a.home.code-1)
    // if already play on previous half wait until next half  (19 - 19)
    // previous match and next not against same team
    if (previousMatchDay> 0 && ((previousMatchDay <= 19 && day <= 19) || (previousMatchDay > 19 && day > 19) || Math.abs(previousMatchDay-day) <= 2)) {
      false
    }
    else {
      if (Set(25, 28, 31, 34).contains(day)) {
        !((UCL.contains(a.home.name) && UEL(a.visitor.name))
          || (UEL.contains(a.home.name) && UCL(a.visitor.name))
          || UCL.contains(a.home.name) && UCL(a.visitor.name))
      }
      else if (a.home.name == "GRA") {
        day != 4
      } else if (a.home.name == "ATH") {
        day != 38
      } else if (a.home.name == "ESP") {
        day != 10
      } else /*if (a.home.name == "BAR") {
      day != 10
    } else*/ {
        true
      }
    }
  }

  def rules(a: Match, day: Int) = (b: Match) => {
    (a.home.name, b.home.name) match {
      case ("RMA", "ATM") => !Set(19).contains(day)
      case ("BAR", "ESP") => !Set(1, 2, 3).contains(day)
      case ("BET", "SEV") => Set(1, 2, 3, 18, 19).contains(day)
      case ("VAL", "LEV") => Set(9).contains(day) && !Set(1, 2, 3, 18, 19).contains(day)
      case _ => true
    }
  }
}
