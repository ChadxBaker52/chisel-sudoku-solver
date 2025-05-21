package sudoku

import chisel3._
import chisel3.util._

class SudokuProcessor(gridSize: Int) extends Module {
    val io = IO(new Bundle {
        val inGrid = Input(Vec(gridSize*gridSize, UInt(gridSize.W)))
        val outGrid = Output(Vec(gridSize*gridSize, UInt(gridSize.W)))
    })

    // Set Condidate Masks
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
                val boxInd = row + (col / 3)
                subMask(boxInd) := subMask(boxInd) | cell
            }
        }

        (rowMask, colMask, subMask)
    }
    
    // Update Candidates and find Singles
    def getSingles(cells: Vec[UInt], rowMask: Vec[Uint], colMask: Vec[Uint], subMask: Vec[Uint]): Vec[UInt] = {
        val updatedCells = WireInit(VecInit(Seq.fill(9*9)(0.U(gridSize.W))))

        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val boxInd = row + (col / 3)
                val cell = io.inGrid((row*9) + col)
                val finalMask = ~(rowMask(row) | colMask(col) | subMask(boxInd))

                updatedCells((row*9) + col) := cell & finalMask
            }
        }

        updatedCells
    }


    val (rowMask, colMask, subMask) = setCandidates()
    io.outGrid := getSingles(io.inGrid, rowMask, colMask, subMask)
}