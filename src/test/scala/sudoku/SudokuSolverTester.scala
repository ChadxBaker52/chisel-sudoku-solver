package sudoku

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import sudoku.SudokuTestUtils.chooseBackend

class SudokuSolverTester extends AnyFlatSpec with ChiselScalatestTester {
  // Helper to decode a 9-bit one-hot back to 0–9
  def oneHotToInt(v: BigInt): Int = {
    if (v == 0x1FF) 0
    else if ((v & (v - 1)) == 0 && v != 0) v.bitLength
    else 0
  }

  // Helper to encode 0–9 into a 9-bit one-hot
  def intToOneHot(d: Int): UInt = {
    if (d >= 1 && d <= 9) (1 << (d-1)).U(9.W)
    else "b111111111".U
  }

  // Print for debugging
  def printGrid(grid: Vec[UInt]): Unit = {
    println("=== Output Sudoku Grid ===")
    grid.grouped(9).foreach { row =>
      println(row.map(r => oneHotToInt(r.peek().litValue)).mkString(" "))
    }
  }

  // Scala reference solver
  def simulateSolver(puzzle: Array[Int]): Array[Int] = {
    // first apply singles until no change
    var curr = puzzle
    var changed = true
    while (changed) {
      val next = SudokuProcessorModel.solveSingles(curr)
      changed = next.zip(curr).exists { case (a, b) => a != b }
      curr = next
    }
    // then DFS
    SudokuProcessorModel.dfsSolveSudoku(curr).getOrElse {
      throw new Exception("Reference DFS failed")
    }
  }

  behavior of "SudokuSolver"
  it should "solve this puzzle" in {
    test(new SudokuSolver(9))
      .withAnnotations(chooseBackend())
      .apply{ dut =>
        // turn off prints in the DUT or guard them in your code!
        dut.clock.setTimeout(0)

        // define puzzle
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
        val expected = simulateSolver(puzzle)

        // poke inputs
        for (i <- 0 until 81) {
          dut.io.inGrid(i).poke(intToOneHot(puzzle(i)))
        }
        dut.io.start.poke(true.B)
        dut.clock.step()
        dut.io.start.poke(false.B)
        
        val maxTries = 1
        var tries = 0
        val step = 1000

        // wait for done in 1k‐cycle steps
        while (!dut.io.done.peek().litToBoolean && tries < maxTries) {
          dut.clock.step(step)
          if (tries % step == 0)
            printGrid(dut.io.outGrid)
          tries += step
        }
        assert(dut.io.done.peek().litToBoolean, "Solver did not finish in time")
        
        val cycles = dut.io.cycles.peek().litValue
        println(s"cycles: $cycles")

        // collect & compare
        // val out = (0 until 81).map { i =>
        //   oneHotToInt(dut.io.outGrid(i).peek().litValue)
        // }
        // out.zipWithIndex.foreach { case (got, idx) =>
        //   assert(got == expected(idx),
        //     s"Mismatch at cell $idx: got $got, expected ${expected(idx)}")
        // }
      }
  }
}
