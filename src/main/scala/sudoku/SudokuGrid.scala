package sudoku

import chisel3._
import chisel3.util._

/**
  * Builds Sudoku Grid Template
  */
class SudokuGrid(val gridSize: Int = 9) extends Bundle {
    // 81 9-bit vectors representing grid
    val cells = Vec(gridSize * gridSize, UInt(gridSize.W))

    def apply(i: Int): UInt = cells(i)

    // Set a value using an integer which is then converted into OH
    def setVal(i: Int, x: Int): Unit = {
        require(x >= 1 && x <= gridSize)
        cells(i) := UIntToOH(((x - 1).U))
    }

    // Sets all values of grid to a 1, resetting it
    def resetGrid(): Unit = {
        for (i <- 0 until (gridSize * gridSize)) {
            setVal(i, 1)
        }
    }
}

class SudokuGridModule(val gridSize: Int = 9) extends Module {
    val io = IO(new Bundle {
        val outGrid = Output(new SudokuGrid(gridSize))
    })

    val grid = Reg(new SudokuGrid(gridSize))

    io.outGrid := grid
}