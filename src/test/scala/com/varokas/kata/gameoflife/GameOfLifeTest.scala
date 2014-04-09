package com.varokas.kata.gameoflife

import org.scalatest.{Matchers, FunSpec}

class GameOfLifeTest extends FunSpec with Matchers {
  describe("Cell") {
    it("rel() should be able to produce relative cell") {
      Cell(0,0).rel(1,-1) shouldBe Cell(1, -1)
      Cell(2,-5).rel(-2,5) shouldBe Cell(0, 0)
    }

    it("neighbours() should be able to produce relative cell") {
      Cell(0,0).neighbours().toSet shouldBe Set(
        Cell(-1,-1), Cell( 0,-1), Cell(+1,-1),
        Cell(-1, 0),              Cell(+1, 0),
        Cell(-1,+1), Cell( 0,+1), Cell(+1,+1)
      )
    }
  }

  describe("Board") {
    val board = new Board(Set())

    describe("liveCellShouldLive()") {
      it("Any live cell with fewer than two live neighbours dies, as if caused by under-population") {
        (0 until 2).foreach(Rule.shouldLiveForLiveCell(_) shouldBe (false))
      }
      it("Any live cell with two or three live neighbours lives on to the next generation.") {
        (2 to 3).foreach(Rule.shouldLiveForLiveCell(_) shouldBe (true))
      }
      it("Any live cell with more than three live neighbours dies, as if by overcrowding.") {
        (4 to 8).foreach(Rule.shouldLiveForLiveCell(_) shouldBe (false))
      }
    }
    describe("deadCellShouldLive()") {
      it("Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.") {
        Rule.shouldLiveForDeadCell(3) shouldBe (true)
        (1 to 2).foreach(Rule.shouldLiveForDeadCell(_) shouldBe (false))
        (4 to 8).foreach(Rule.shouldLiveForDeadCell(_) shouldBe (false))
      }
    }
  }

  describe("Game of Life") {
    it("should be able to do still lives") {
      val board = Board.create(
        "XX\n" +
        "XX\n"
      );

      board.advance() shouldEqual board
    }

    it("should be able to do Oscillators") {
      val board = Board.create(
        "X\n" +
        "X\n" +
        "X\n"
      );

      board.advance().advance() shouldEqual board
    }
  }
}
