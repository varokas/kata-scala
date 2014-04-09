package com.varokas.kata.gameoflife


case class Cell(x:Int, y:Int) {
  def rel(dX:Int, dY:Int) = new Cell(x+dX, y+dY)
  def neighbours():Iterable[Cell] = {
    for(
      x <- (-1 to 1); y <- (-1 to 1)
      if rel(x,y) != this
    )
    yield rel(x,y)
  }
}

object Rule {
  def shouldLiveForLiveCell(neighbours:Int):Boolean = (2 to 3).contains(neighbours)
  def shouldLiveForDeadCell(neighbours:Int):Boolean = (neighbours == 3)

  def forCellState(alive:Boolean):(Int => Boolean) = if(alive) shouldLiveForLiveCell else shouldLiveForDeadCell
}

class Board(val liveCells:Set[Cell]) {
  def advance():Board = {
    val cellsToLiveNextTurn =
      allCellsToCompute(liveCells)
        .filter(
          c => Rule.forCellState(isAlive(c)).apply(countNeighbours(c))
        )

    return new Board(cellsToLiveNextTurn)
  }

  private def isAlive(c:Cell):Boolean = liveCells.contains(c)
  private def countNeighbours(c:Cell):Int = c.neighbours().count( n => liveCells.contains(n) )

  private def allCellsToCompute(liveCells: Iterable[Cell]): Set[Cell] = {
    liveCells.flatMap(c => c.neighbours().toSet + c).toSet
  }


  override def toString():String = {
    if(liveCells.size == 0) return ""

    val allX = liveCells.map( lc => lc.x )
    val allY = liveCells.map( lc => lc.y )

    val xs = allX.min to allX.max
    val ys = allY.min to allY.max

    val str = ys.map( y => getRow(xs, y) ).fold("")(_ + _)

    return str
  }

  private def getRow(xs:Iterable[Int], y:Int) = {
    xs.map( x => if(isAlive(Cell(x,y))) "X" else "." )
      .fold("")( (acc,n) => acc + n ) + "\n"
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Board]

  override def equals(other: Any): Boolean = other match {
    case that: Board =>
      (that canEqual this) &&
        liveCells == that.liveCells
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(liveCells)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Board {
  def create(str:String):Board = {
    val lines = str.split("\n")

    val liveCells = for(
      x <- 0 until lines.size;
      y <- 0 until lines(x).length
      if (lines(x)(y) == 'X')
    ) yield Cell(x,y)

    return new Board(liveCells.toSet)
  }
}




