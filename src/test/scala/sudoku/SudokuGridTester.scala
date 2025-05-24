package sudoku

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SudokuPrintTest extends AnyFlatSpec with ChiselScalatestTester {
  "SudokuGridModule" should "print values row by row" in {
    test(new SudokuModule(9)) { dut =>
      for(i <- 0 until 81) {
        dut.io.inGrid(i).poke(1.U)         // bit-0 = 1
      }
      // start = false → done should be false
      // dut.io.start.poke(false.B)
      // dut.clock.step(1)

      // Now pulse start
      // dut.io.start.poke(true.B)
      // dut.clock.step(1)

      // And outGrid should exactly mirror inGrid
      for(i <- 0 until 81) {
        dut.io.outGrid(i).expect(1.U)
      }
    }
  }
}

