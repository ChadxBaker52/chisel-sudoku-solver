package sudoku

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SudokuSolverTester extends AnyFlatSpec with ChiselScalatestTester {
  def oneHotToInt(oneHot: BigInt): Int = {
    if (oneHot == 0x1FF) 0
    else if ((oneHot & (oneHot - 1)) == 0 && oneHot != 0) {
      oneHot.bitLength
    } else {
      0
    }
  }

  def intToOneHot(d: Int): UInt = {
    if (d >= 1 && d <= 9) (1 << (d - 1)).U(9.W)
    else "b111111111".U(9.W)
  }

  def printGrid(grid: Vec[UInt]) = {
    println("=== Output Sudoku Grid ===")
    for (i <- 0 until 81) {
      val out = grid(i).peek().litValue
      val digit = oneHotToInt(out)
      print(s"$digit ")
      if ((i + 1) % 9 == 0) println()
    }
  }

  def simulateSolver(puzzle: Array[Int]): Array[Int] = {
    var currentPuzzle = puzzle
    var changed = true

    while (changed) {
      val next = SudokuProcessorModel.solveSingles(currentPuzzle)
      changed = next.zip(currentPuzzle).exists { case (a, b) => a != b }
      currentPuzzle = next
    }

    SudokuProcessorModel.dfsSolveSudoku(currentPuzzle).getOrElse {
      throw new RuntimeException("Hardware-equivalent DFS failed")
    }
  }

  behavior of "SudokuSolver" 
  it should "Solve this puzzle" in {
    test(new SudokuSolver()) { dut =>
      dut.clock.setTimeout(0)
      val puzzle = Array(
        4,0,0, 0,7,8, 0,0,0,
        0,5,1, 3,9,0, 0,0,0,
        0,6,0, 4,2,0, 0,0,0,
        0,0,8, 0,0,0, 0,0,0,
        9,0,2, 0,0,0, 3,0,0,
        0,0,5, 6,0,0, 0,0,1,
        0,0,0, 5,0,0, 0,0,6,
        0,0,0, 2,0,0, 1,0,4,
        0,0,3, 0,0,0, 0,0,7
      )

      val solution = simulateSolver(puzzle)

      // Apply inputs
      for (i <- 0 until 81) {
        dut.io.inGrid(i).poke(intToOneHot(puzzle(i)))
      }
      dut.io.start.poke(true.B)
      dut.clock.step(1)
      dut.io.start.poke(false.B)

      // Wait for solver to finish
      var maxCycles = 100000
      while (dut.io.done.peek().litToBoolean == false && maxCycles > 0) {
        dut.clock.step(1)
        maxCycles -= 1

        if (maxCycles % 100 == 0 ) {
          printGrid(dut.io.outGrid)
        }
      }

      assert(dut.io.done.peek().litToBoolean, "Solver did not finish in time")

      // Read back solution
      val output = (0 until 81).map(i => oneHotToInt(dut.io.outGrid(i).peek().litValue)).toArray

      // Check match
      for (i <- 0 until 81) {
        assert(output(i) == solution(i), s"Mismatch at index $i: expected ${solution(i)}, got ${output(i)}")
      }
    }
  }
}
  