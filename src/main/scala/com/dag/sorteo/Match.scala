package com.dag.sorteo

class Match (val home: Team, val visitor: Team) {

  //  if matches doesn't intersect
  def compatibleOnFixture(a: Match): Boolean = home.code != a.home.code && home.code != a.visitor.code && visitor.code != a.home.code && visitor.code != a.visitor.code

  override def toString = s"${home.name}-${visitor.name}"
}
