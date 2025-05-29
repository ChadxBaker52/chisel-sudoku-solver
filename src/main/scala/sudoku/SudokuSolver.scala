package sudoku

import chisel3._
import chisel3.util._

class SudokuSolver() extends Module {
    val io = IO(new Bundle {
        val inGrid  = Input(new SudokuGrid(9))
        val load    = Input(Bool())
        val start   = Input(Bool())
        val done    = Output(Bool())
        val outGrid = Output(new SudokuGrid(9))
    })

    // STORAGE
    val grid = Reg(new SudokuGrid(9))

    when(io.load) {
        grid := io.inGrid
    }

    // CONTROLLER
    val controller = Module(new SudokuController())
    // add io connections

    // PROCESSOR
    val proc = Module(new SudokuProcessor())
    // add io connections

}