package sudoku

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SudokuControllerTester extends AnyFlatSpec with ChiselScalatestTester {

  behavior of "SudokuController" 
  it should "Go through all states (Idle -> Single -> Hidden -> DFS -> Done)" in {
    test(new SudokuController()) { dut =>

      // Idle
      dut.io.start.poke(false.B)
      dut.io.changed.poke(false.B)
      dut.io.solved.poke(false.B)

      dut.io.done.expect(false.B)
      dut.io.mode.expect(0.U)
      dut.io.loadGrid.expect(true.B)

      dut.clock.step(1)       // Next cycle

      // Idle -> Single
      dut.io.start.poke(true.B)
      dut.io.changed.poke(false.B)
      dut.io.solved.poke(false.B)

      dut.io.done.expect(false.B)
      dut.io.mode.expect(0.U)
      dut.io.loadGrid.expect(true.B)

      dut.clock.step(1)       // Next cycle

      // Single -> DFS
      dut.io.start.poke(false.B)
      dut.io.changed.poke(false.B)
      dut.io.solved.poke(false.B)

      dut.io.done.expect(false.B)
      dut.io.mode.expect(1.U)
      dut.io.loadGrid.expect(false.B)

      dut.clock.step(1)       // Next cycle

      // DFS -> Done
      dut.io.start.poke(false.B)
      dut.io.changed.poke(false.B)
      dut.io.solved.poke(true.B)

      dut.io.done.expect(false.B)
      dut.io.mode.expect(2.U)
      dut.io.loadGrid.expect(false.B)

      dut.clock.step(1)       // Next cycle

      dut.io.done.expect(true.B)
      dut.io.mode.expect(2.U)
      dut.io.loadGrid.expect(false.B)
    }
  }
}

