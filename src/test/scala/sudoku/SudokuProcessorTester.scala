package sudoku

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SudokuProcessorTester extends AnyFlatSpec with ChiselScalatestTester {
  "SudokuProcessor" should "test dfs on empty grid" in {
    test(new SudokuProcessor()) { dut =>
      def oneHotToDigit(oneHot: BigInt): Int =
        if (oneHot == 0x1FF) 0
        else oneHot.bitLength  // assumes only one bit is set

      def printGrid() = {
        println("=== Output Sudoku Grid ===")
        for (i <- 0 until 81) {
          val out = dut.io.outGrid(i).peek().litValue
          val digit = oneHotToDigit(out)
          print(s"$digit ")
          if ((i + 1) % 9 == 0) println()
          // dut.io.outGrid(i).expect(correct(i))
        }
      }

      dut.clock.setTimeout(0)
      val puzzle  = Array.fill(81)("b111111111".U(9.W))
      val correct = (0 until 81).map(i =>
        if (i == 0) 1.U(9.W) else "b111111111".U(9.W)
      ).toArray

      for (i <- 0 until 81) {
        dut.io.inGrid(i).poke(puzzle(i))
      } 
      dut.io.mode.poke(0.U)
      dut.clock.step(3)

      dut.io.mode.poke(1.U)
      dut.clock.step(3)

      dut.io.mode.poke(2.U)
      var cycles = 0
      val maxCycles = 10000 // Prevent infinite loop

      while (!dut.io.done.peek().litToBoolean && cycles < maxCycles) {
        dut.clock.step(1)
        cycles += 1
      }

      if (cycles >= maxCycles) {
        printGrid()
        fail("Solver did not complete in time")
      } else {
        println(s"Solved in $cycles cycles")
      }
      printGrid()
    }
  }
}