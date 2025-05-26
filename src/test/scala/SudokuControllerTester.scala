package sudoku

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class SudokuControllerTester extends AnyFlatSpec with ChiselScalatestTester {
  "SudokuController" should "Go through all states (Idle -> Single -> Hidden -> DFS -> Done)" in {
    test(new SudokuController()) { dut =>

        // Idle
        dut.io.start.poke(false.B)
        dut.io.changed.poke(false.B)
        dut.io.solved.poke(false.B)

        dut.io.done.expect(false.B)
        dut.io.mode.expect(0.U)

        dut.clock.step(1)       // Next cycle

        // Idle -> Single
        dut.io.start.poke(true.B)
        dut.io.changed.poke(false.B)
        dut.io.solved.poke(false.B)

        dut.io.done.expect(false.B)
        dut.io.mode.expect(0.U)

        dut.clock.step(1)       // Next cycle

        // Single -> Hidden
        dut.io.start.poke(false.B)
        dut.io.changed.poke(false.B)
        dut.io.solved.poke(false.B)

        dut.io.done.expect(false.B)
        dut.io.mode.expect(1.U)

        dut.clock.step(1)       // Next cycle

        // Hidden -> DFS
        dut.io.start.poke(false.B)
        dut.io.changed.poke(false.B)
        dut.io.solved.poke(false.B)

        dut.io.done.expect(false.B)
        dut.io.mode.expect(2.U)

        dut.clock.step(1)       // Next cycle

        // DFS -> Done
        dut.io.start.poke(false.B)
        dut.io.changed.poke(false.B)
        dut.io.solved.poke(false.B)

        dut.io.done.expect(false.B)
        dut.io.mode.expect(3.U)
    }
  }
}

