package com.dag.sorteo

import java.security.SecureRandom

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

  def calculate(matches: List[Match], c: Calendar, days: List[Int]): List[Operation] = {
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
      List()
    }
    else {
      println(s"Calculating day ${days.head}")
      val mm = Utils.shuffle(matches)
      val dd = Utils.shuffle(days)
      val day = dd.head
      List(CalculateFixtures(mm.filter(c.rulesDaily(_, day)), c, day, 10, mm, dd.tail))
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
