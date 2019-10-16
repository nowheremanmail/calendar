package com.dag.sorteo

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")

    val teams = (1 to 20).map { i => new Team(i + "", i + "") }

    val matches = teams.map(a => teams.filter(c => c != a).map (b => new Match (a, b)))

  }
}
