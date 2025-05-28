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
    val unsolvedCount = RegInit(VecInit(Seq.fill(27)(VecInit(Seq.fill(9)(0.U(4.W))))))

    // Set Candidates and find singles
    def getCandidatesAndSingles(cells: Vec[UInt]): Vec[UInt] = {
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
        val changed = WireInit(false.B)

        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val boxInd = row + (col / 3)
                val cell = cells((row*9) + col)
                val finalMask = ~(rowMask(row) | colMask(col) | subMask(boxInd))
                val prune = cell & finalMask

                updatedCells((row*9) + col) := prune

                when (prune =/= cell) {
                    changed := true.B
                }
            }
        }

        updatedCells
    }

    // Find Hidden Singles
    def getHiddenSingles(cells: Vec[UInt]): Vec[UInt] = {
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
                    unsolvedCount(row)(i) := unsolvedCount(row)(i) + 1.U
                }
            }
        }

        // checking columns
        for (col <- 9 until 18) {
            val mask = colMask(col).asBools
            for (i <- 0 until 9) {
                when (mask(i)) {
                    unsolvedCount(col)(i) := unsolvedCount(col)(i) + 1.U
                }
            }
        }

        // checking subgrids
        for (sub <- 18 until 27) {
            val mask = subMask(sub).asBools
            for (i <- 0 until 9) {
                when (mask(i)) {
                    unsolvedCount(sub)(i) := unsolvedCount(sub)(i) + 1.U
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

        val nextCells = WireInit(VecInit(Seq.fill(9*9)(0.U(9.W))))
        val changed = RegInit(false.B)

        // Updating cells by row
        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val cell = cells((row*9) + col)
                val mask = hRowMask(row)
                when ((mask & cell) =/= 0.U) {
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
                when ((mask & cell) =/= 0.U) {
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
                when ((mask & cell) =/= 0.U) {
                    nextCells((row*9) + col) := mask
                    changed := true.B
                }
            }
        }

        nextCells
    }

    def dfs(start: Bool, empty: Bool, valid: Bool, candidate: Bool, traversed: Bool) = {
        val cellIdx = RegInit(0.U(log2Ceil(81).W))
        val cell = io.inGrid(cellIdx)

        object dfsState extends ChiselEnum {
            val idle, findEmpty, checkValid, backtrack, done = Value
        }

        val state = RegInit(dfsState.Idle)

        // Go to first cell
        switch (state) {
            is (dfsState.idle) {
                // initialize everything
                when (start) {
                    state := findEmpty
                }
            }
            is (dfsState.findEmtpy) {
                when (empty) {
                    state := dfsState.checkValid
                } .elsewhen (~traversed) {
                    state := dfsState.findEmpty
                } .otherwise {
                    state := dfsState.done
                }
            }
            is (dfsState.checkValid) {
                when (valid) {
                    state := dfsState.findEmpty
                } .elsewhen (candidate) {
                    state := dfsState.checkValid
                } .otherwise {
                    state := dfsState.backtrack
                }
            }
            is (dfsState.backtrack) {
                when (candidate) {
                    state := dfsState.checkValid
                } .elsewhen (cellIdx === 0.U) {   // check when it has backtracked too far
                    state := dfsState.done
                } .otherwise {
                    state := dfsState.backtrack
                }
            }
            // is (dfsState.done) {

            // }
        }
    }

    val singleCells = getCandidatesAndSingles(io.inGrid)
    val hiddenCells = getHiddenSingles(io.inGrid)

    val newCells = 0.U.asTypeOf(new SudokuGrid(gridSize))


    when (io.mode === 0.U) {
        newCells := singleCells
    } .elsewhen (io.mode === 1.U) (
        newCells := hiddenCells
    ) 
    // DFS
    // .elsewhen (mode === 2.U) {

    // }

    io.changed := false.B
    io.outGrid := newCells
}