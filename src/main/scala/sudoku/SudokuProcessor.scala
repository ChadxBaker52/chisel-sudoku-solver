package sudoku

import chisel3._
import chisel3.util._

class SudokuProcessor() extends Module {
    // Find candidates and singles
    def setCandidates(cells: Vec[UInt]): (Vec[UInt], Vec[UInt], Vec[UInt]) = {
        // 27 9-bit vectors
        val rowMask = WireInit(VecInit(Seq.fill(9)(0.U(gridSize.W))))
        val colMask = WireInit(VecInit(Seq.fill(9)(0.U(gridSize.W))))
        val subMask = WireInit(VecInit(Seq.fill(9)(0.U(gridSize.W))))

        // initializing Masks
        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val cell = cells((row*9) + col)
                // Row
                rowMask(row) := rowMask(row) | cell
                // Col
                colMask(col) := colMask(col) | cell
                // Sub
                val boxIdx = ((row / 3) * 3) + (col / 3)
                subMask(boxIdx) := subMask(boxIdx) | cell
            }
        }

        (rowMask, colMask, subMask)
    }
}