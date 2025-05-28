package sudoku

import chisel3._
import chisel3.util._

class SudokuProcessor() extends Module {
    val io = IO(new Bundle {
        val inGrid = Input(Vec(9*9, UInt(9.W)))
        val mode = Input(UInt(2.W))
        val done = Output(Bool())
        val outGrid = Output(Vec(9*9, UInt(9.W)))
    })
    // 
    val grid = io.inGrid
    // copy of grid for DFS
    val nextGrid = RegInit(VecInit(Seq.fill(9*9)(0.U(9.W))))
    // main grid
    val refGrid = RegInit(VecInit(Seq.fill(9*9)(0.U(9.W))))

    // hate this please change!!!
    val sampled = RegInit(true.B)
    when (sampled) {
        refGrid  := io.inGrid
        nextGrid := io.inGrid
        sampled  := false.B
    }

    // 27 9-bit vectors for Candidates
    val rowMask = RegInit(VecInit(Seq.fill(9)(0.U(9.W))))
    val colMask = RegInit(VecInit(Seq.fill(9)(0.U(9.W))))
    val subMask = RegInit(VecInit(Seq.fill(9)(0.U(9.W))))

    // 27 Vectors full of 9 4-bit vectors used for Hidden Singles
    // val unsolvedCount = RegInit(VecInit(Seq.fill(27)(VecInit(Seq.fill(9)(0.U(4.W))))))

    val cellIdx = RegInit(0.U(log2Ceil(81).W))
    val cell = grid(cellIdx)
    val candPtr = RegInit(VecInit(Seq.fill(81)(0.U(4.W))))
    val stack = Reg(Vec(81, UInt(7.W)))
    // stack pointer: how many entries are in the stack right now
    val sp = RegInit(0.U(7.W))
    val dfsDone = RegInit(false.B)

    // Set empty, valid, candidate, traversed
    val empty = ~cell.isOneHot
    val candOH = UIntToOH(candPtr(cellIdx))(8,0)
    val valid = isValidCell(cellIdx, candOH)
    val candidate = candPtr(cellIdx) === 8.U
    val traversed = cellIdx === 81.U

    // Set Candidates
    def setCandidates() = {
        // new 27 9-bit vectors for Candidates
        val nextRowMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))
        val nextColMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))
        val nextSubMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))

        // initializing candidate masks
        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val cell = grid((row*9) + col)
                when (cell.isOneHot){           // only want to OR singles
                    // Row
                    nextRowMask(row) := nextRowMask(row) | cell
                    // Col
                    nextColMask(col) := nextColMask(col) | cell
                    // Sub
                    val boxInd = row + (col / 3)
                    nextSubMask(boxInd) := nextSubMask(boxInd) | cell
                }
            }
        }

        rowMask := nextRowMask
        colMask := nextColMask
        subMask := nextSubMask
    }

    // find singles
    def pruneSingles() = {
        // first set candidates
        setCandidates()

        // prune mask by or'ing row, col, and sub together
        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val boxInd = row + (col / 3)
                val cell = grid((row*9) + col)
                val finalMask = ~(rowMask(row) | colMask(col) | subMask(boxInd))
                val prunedMask = cell & finalMask

                nextGrid((row*9) + col) := prunedMask
            }
        }
        // update refGrid with new values
        refGrid := nextGrid
    }

    def getHiddenSingles() = {
        // initialize counters
        val unsolvedCount = WireInit(VecInit(Seq.fill(27)(VecInit(Seq.fill(9)(0.U(4.W))))))

        // Create hidden masks
        val hRowMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))
        val hColMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))
        val hSubMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))

        // Counting cells
        // ----------------------------------------------------
        // Counting cells in each row
        for (row <- 0 until 9) {
            val mask = rowMask(row).asBools
            for (i <- 0 until 9) {
                when (mask(i)) {
                    unsolvedCount(row)(i) := unsolvedCount(row)(i) + 1.U
                }
            }
        }

        // Counting cells in each column
        for (col <- 9 until 18) {
            val mask = colMask(col-9).asBools
            for (i <- 0 until 9) {
                when (mask(i)) {
                    unsolvedCount(col)(i) := unsolvedCount(col)(i) + 1.U
                }
            }
        }

        // Counting cells in each subgrid
        for (sub <- 18 until 27) {
            val mask = subMask(sub-18).asBools
            for (i <- 0 until 9) {
                when (mask(i)) {
                    unsolvedCount(sub)(i) := unsolvedCount(sub)(i) + 1.U
                }
            }
        }
        // ----------------------------------------------------

        // Checking for singles
        // ----------------------------------------------------
        // checking rows for singles
        for (row <- 0 until 9) {
            val bits = Wire(Vec(9, Bool()))
            for (i <- 0 until 9) {
                bits(i) := unsolvedCount(row)(i) === 1.U
            }
            hRowMask(row) := bits.asUInt  
        }

        // checking columns for singles
        for (col <- 9 until 18) {
            val bits = Wire(Vec(9, Bool()))
            for (i <- 0 until 9) {
                bits(i) := unsolvedCount(col)(i) === 1.U
            }
            hColMask(col-9) := bits.asUInt
        }

        // checking subgrids for singles
        for (sub <- 18 until 27) {
            val bits = Wire(Vec(9, Bool()))
            for (i <- 0 until 9) {
                bits(i) := unsolvedCount(sub)(i) === 1.U
            }
            hSubMask(sub-18) := bits.asUInt
        }
        // ----------------------------------------------------

        // Updating cells
        // ----------------------------------------------------
        // Updating cells by row
        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val cell = grid((row*9) + col)
                val mask = hRowMask(row)
                when ((mask & cell) =/= 0.U) {
                    nextGrid((row*9) + col) := mask
                }
            }
        }

        // Updating cells by column
        for (col <- 0 until 9) {
            for (row <- 0 until 9) {
                val cell = grid((row*9) + col)
                val mask = hColMask(col)
                when ((mask & cell) =/= 0.U) {
                    nextGrid((row*9) + col) := mask
                }
            }
        }

        // Updating cells by sub
        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val cell = grid((row*9) + col)
                val sub = row + (col / 3)
                val mask = hSubMask(sub)
                when ((mask & cell) =/= 0.U) {
                    nextGrid((row*9) + col) := mask
                }
            }
        }
        // ----------------------------------------------------

        // update refGrid with new values
        refGrid := nextGrid
    }

    def isValidCell(idx: UInt, candOH: UInt): Bool = {
        val row = idx / 9.U
        val col = idx % 9.U
        val sub = row + (col/3.U)
        // see if candidate is already in row, col, or sub
        !( (rowMask(row) & candidateBit).orR ||
           (colMask(col) & candidateBit).orR ||
           (subMask(sub) & candidateBit).orR )
    }

    def dfs() = {
        object dfsState extends ChiselEnum {
            val idle, findEmpty, checkValid, backtrack, done = Value
        }

        val state = RegInit(dfsState.Idle)

        // Go to first cell
        switch (state) {
            is (dfsState.idle) {
                // initialize everything
                cellIdx := 0.U
                sp := 0.U
                for (i <- 0 until 81) candPtr(i) := 0.U

                // see if cell is empty
                state   := dfsState.findEmpty
            }
            is (dfsState.findEmtpy) {
                when (empty) {
                    // found empty cell, see if valid
                    state := dfsState.checkValid
                } .elsewhen (~traversed) {
                    // try next cell
                    cellIdx := cellIdx + 1.U
                } .otherwise {
                    // no more empty cells = solved
                    state := dfsState.done
                }
            }
            is (dfsState.checkValid) {
                when (valid) {
                    nextGrid := candOH
                    stack(sp) := cellIdx
                    sp := sp + 1.U

                    candPtr(cellIdx) := 0.U

                    // go to next cell
                    cellIdx := cellIdx + 1.U

                    // see if next cell is empty
                    state := dfsState.findEmpty
                } .elsewhen (candidate) {
                    // go to next candidate
                    candPtr(cellIdx) := candPtr(cellIdx) + 1.U
                    // see if valid
                    state := dfsState.checkValid
                } .otherwise {
                    // out of candidates
                    state := dfsState.backtrack
                }
            }
            is (dfsState.backtrack) {
                when (sp === 0.U) {
                    // back tracked too far = failed
                    state := dfsState.done
                } .otherwise {
                    // go to previous cell
                    sp := sp - 1.U
                    val prev = stack(sp)

                    // restore old val
                    nextGrid(prev) := refGrid(prev)

                    candPtr(prev) := candPtr(prev) + 1.U

                    cellIdx := prev
                    when (candidate) { // if already guessing 9th candidate
                        state := dfsState.checkValid
                    } .otherwise {
                        state := dfsState.backtrack
                    }
                }
            }
            is (dfsState.done) {
                dfsDone := true.B
            }
        }
    }

    when (io.mode === 0.U) {
        pruneSingles()
    } .elsewhen (io.mode === 1.U) (
        getHiddenSingles()
    ) .elsewhen (mode === 2.U) {
        dfs()
    }
    io.done = dfsDone
    io.outGrid := nextGrid
}