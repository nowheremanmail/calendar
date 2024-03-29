package com.dag.sorteo

import java.io.{File, PrintWriter}
import java.security.SecureRandom

import com.dag.sorteo.MainRecursive.{calculateFixtures, writer}

import scala.collection.mutable.ArrayBuffer
import scala.collection.{BuildFrom, mutable}

abstract class Operation

case class Calculate(matches: List[Match], c: Calendar, days: List[Int]) extends Operation

case class CalculateFixtures(fixturesMatches: List[Match], c: Calendar, day: Int, value: Int, matches: List[Match], days: List[Int]) extends Operation

object Main {
  var M = 0

  def calculateFixtures(fixturesMatches: List[Match], c: Calendar, day: Int, value: Int, matches: List[Match], days: List[Int]): List[Operation] = {
    if (value == 0) {
      //println(s"Calculating day ${day} step ${value} , next")
      List(Calculate(matches, c, days))
    }
    else if (fixturesMatches.isEmpty || fixturesMatches.length < value) { //} || distinctTeams(fixturesMatches) < value * 2) {
      //println(s"Calculating day ${day} step ${value} , break")
      List()
    }
    else {
      //println(s"Calculating day ${day} step ${value}")

      val mm = fixturesMatches.zipWithIndex
      mm.map(a => {
        val possibleFixturesMatches = mm
          .filter(b => b._2 > a._2).map(b => b._1)
          .filter(b => b.compatibleOnFixture(a._1))
          .filter(c.rules(a._1, day))
        val allRemainMatches = matches
          .filter(b => a._1 != b)
        val cc = c.newWith(day, a._1)

        CalculateFixtures(possibleFixturesMatches, cc, day, value - 1, allRemainMatches, days)
      })
    }
  }

  def distinctTeams(matches: List[Match]) = matches.flatMap(a => Set(a.home.code, a.visitor.code)).length
  val writer = new PrintWriter(new File("calendars.txt" ))

  def calculate(matches: List[Match], c: Calendar, days: List[Int]): List[Operation] = {
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
        writer.println(msg)
        writer.flush()
      }
      else {
        //println("no more matches, pull")
      }
      List()
    }
    else {
      val mm = Utils.shuffle(matches)

      // order days in order of number of possible options
      val sortedDaysByComplexity = Utils.shuffle(days).map(day => (day , mm.filter(c.rulesDaily(_, day)))).sortBy(a=>a._2.size)
      val dayData = sortedDaysByComplexity.head
      println(s"Calculating day ${dayData._1}")

      List(CalculateFixtures(dayData._2, c, dayData._1, 10, mm, sortedDaysByComplexity.tail.map(a=>a._1)))
    }
  }


  def main(args: Array[String]): Unit = {
    val c = new Calendar
    val teams = c.teams.zipWithIndex.map { case (a, i) => new Team(i + 1, a) }
    val days = (1 to 38).toList
    val matches = teams.flatMap(a => teams.filter(c => c != a).map(b => new Match(a, b))).toList

    //calculate(matches, new Calendar(teams), days);
    val stack = mutable.Queue[Any]()

    stack += Calculate(matches, c, days);

    while (!stack.isEmpty) {
      val current = stack.dequeue()

      val newOperations = current match {
        case Calculate(m, c, days) => calculate(m, c, days)
        case CalculateFixtures(fixturesMatches, c, day, value, matches, days) => calculateFixtures(fixturesMatches, c, day, value, matches, days)
      }

      stack.addAll(newOperations)
    }

  }
}
