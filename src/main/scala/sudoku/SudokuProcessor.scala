package sudoku

import chisel3._
import chisel3.util._

class SudokuProcessor() extends Module {
    val io = IO(new Bundle {
        val inGrid = Input(Vec(9*9, UInt(9.W)))
        val mode = Input(UInt(2.W))
        val changed = Output(Bool())
        val outGrid = Output(Vec(9*9, UInt(9.W)))
    })
    // 27 9-bit vectors for Candidates
    val rowMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))
    val colMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))
    val subMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))

    // 27 Vectors full of 9 4-bit vectors used for Hidden Singles
    val unsolvedCount = RegInit(VecInit(Seq.fill(27, 9)(0.U(4.W))))

    // Set Candidates and find singles
    def getCandidatesAndSingles(cells: Vec[UInt]): (Vec[UInt], Bool) = {
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
    
        // find Singles
        val updatedCells = WireInit(VecInit(Seq.fill(9*9)(0.U(9.W))))
        val changed = WireInit(Bool(false.B))

        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val boxInd = row + (col / 3)
                val cell = cells((row*9) + col)
                val finalMask = ~(rowMask(row) | colMask(col) | subMask(boxInd))
                val prune = cell & finalMask

                updatedCells((row*9) + col) := prune

                when (prune =/= c) {
                    changed := true.B
                }
            }
        }

        (updatedCells, changed)
    }

    // Find Hidden Singles
    def getHiddenSingles(cells: Vec[UInt]): (Vec[UInt], Bool()) = {
        // zero at some point each time function is used
        // for (i <- 0 until 27) {
        //     for (j <- 0 until 9) {
        //         unsolvedCount(i)(j) := 0
        //     }
        // }

        // checking rows
        for (row <- 0 until 9) {
            val mask = rowMask(row).asBools
            for (i <- 0 until 9) {
                when (mask(i)) {
                    unsolvedCount(row)(i) := unsolvedCount(row)(i) + 1
                }
            }
        }

        // checking columns
        for (col <- 9 until 18) {
            val mask = colMask(col).asBools
            for (i <- 0 until 9) {
                when (mask(i)) {
                    unsolvedCount(col)(i) := unsolvedCount(col)(i) + 1
                }
            }
        }

        // checking subgrids
        for (sub <- 18 until 27) {
            val mask = subMask(sub).asBools
            for (i <- 0 until 9) {
                when (mask(i)) {
                    unsolvedCount(sub)(i) := unsolvedCount(sub)(i) + 1
                }
            }
        }

        // to save hardware use old Masks, values dont matter anymore... maybe
        val hRowMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))
        val hColMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))
        val hSubMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))

        // checking rows
        for (row <- 0 until 9) {
            val bits = Wire(Vec(9, Bool()))
            for (i <- 0 until 9) {
                bits(i) := unsolvedCount(row)(i) === 1.U
            }
            hRowMask(row) := bits.asUInt  
        }

        // checking columns
        for (col <- 9 until 18) {
            val bits = Wire(Vec(9, Bool()))
            for (i <- 0 until 9) {
                bits(i) := unsolvedCount(col)(i) === 1.U
            }
            hColMask(col) := bits.asUInt
        }

        // checking subgrids
        for (sub <- 18 until 27) {
            val bits = Wire(Vec(9, Bool()))
            for (i <- 0 until 9) {
                bits(i) := unsolvedCount(sub)(i) === 1.U
            }
            hSubMask(sub) := bits.asUInt
        }

        // if hidden mask and cell mask 
        val nextCells = WireInit(VecInit(Seq.fill(9*9)(0.U(9.W))))
        val changed = WireInit(Bool())

        // Updating cells by row
        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val cell = cells((row*9) + col)
                val mask = hRowMask(row)
                when (mask & cell =/= 0.U) {
                    nextCells((row*9) + col) := mask
                    changed := true.B
                }
            }
        }

        // Updating cells by column
        for (col <- 0 until 9) {
            for (row <- 0 until 9) {
                val cell = cells((row*9) + col)
                val mask = hColMask(col)
                when (mask & cell =/= 0.U) {
                    nextCells((row*9) + col) := mask
                    changed := true.B
                }
            }
        }

        // Updating cells by sub
        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val cell = cells((row*9) + col)
                val sub = row + (col / 3)
                val mask = hSubMask(sub)
                when (mask & cell =/= 0.U) {
                    nextCells((row*9) + col) := mask
                    changed := true.B
                }
            }
        }

        (nextCells, changed)
    }

    val changed = Reg(Bool())
    val newCells = WireInit(VecInit(Seq.fill(9*9)(0.U(9.W))))

    when (mode === 0.U) {
        (newCells, changed) = getCandidatesAndSingles(io.inGrid)
    } .otherwise (mode === 1.U) (
        (newCells, changed) = getHiddenSingles(io.inGrid)
    ) 
    // DFS
    // .otherwise (mode === 2.U) {

    // }

    io.changed := changed
    io.outGrid := newCells
}