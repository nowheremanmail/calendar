package com.dag.sorteo

class Calendar(val teams: List[Team]) {
  val cal = Array.ofDim[Int](20, 20);
  var N = 0;

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

    //println(s"set - ${m.home} - ${m.visitor} = ${d}")

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


  def seeFixtures(M: Int) = {

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

    val msg = JsonUtil.toJson(fixtures)

    if (Cache.previous.contains(msg)) {
      throw new RuntimeException("REPEAT!")
    }
    else {
      Cache.previous.add(msg)
    }

    println(">>>>" + M + " " + msg);

    /*
        for {
          i <- 0 until 38} {
          println(i + " " + fixtures(i))
        }
    */
  }

}
