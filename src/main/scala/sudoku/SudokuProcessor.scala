package sudoku

import chisel3._
import chisel3.util._
import SudokuUtils._

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
    val unsolvedCount = RegInit(VecInit(Seq.fill(27)(VecInit(Seq.fill(9)(0.U(4.W))))))


    // DFS Variables
    val cellIdx = RegInit(0.U(log2Ceil(81).W))
    val cell = nextGrid(cellIdx)
    val candPtr = RegInit(VecInit(Seq.fill(81)(0.U(4.W))))
    val stack = Reg(Vec(81, UInt(7.W)))
    val sp = RegInit(0.U(7.W))
    val dfsDone = RegInit(false.B)

    val empty = ~isOneHot(cell)
    val cand = candPtr(cellIdx)
    val candValid = cand > 0.U && cand <= 9.U
    val safeCand = Mux(candValid, cand - 1.U, 0.U)
    val candOH = UIntToOH(safeCand)(8, 0)
    val valid = isValidCell(cellIdx, candOH)
    val candidate = candPtr(cellIdx) < 8.U
    val traversed = cellIdx === 81.U

    
    object dfsState extends ChiselEnum {
        val idle, findEmpty, checkValid, backtrack, done = Value
    }

    val state = RegInit(dfsState.idle)

    // Set Candidates
    def setCandidates(cGrid: Vec[UInt]) = {
        // new 27 9-bit vectors for Candidates
        val nextRowMask = WireInit(
            VecInit((0 until 9).map { row =>
                val rowCells = (0 until 9).map(col => cGrid(row*9 + col))
                rowCells.map(cell => Mux(isOneHot(cell), cell, 0.U)).reduce(_|_)
            })
        )
        val nextColMask = WireInit(
            VecInit((0 until 9).map { col =>
                val colCells = (0 until 9).map(row => cGrid(row*9 + col))
                colCells.map(cell => Mux(isOneHot(cell), cell, 0.U)).reduce(_|_)
            })
        )
        val nextSubMask = WireInit(
            VecInit((0 until 9).map { sub =>
                val cells = (0 until 9).map { i =>
                    val row = (sub / 3) * 3 + (i / 3)
                    val col = (sub % 3) * 3 + (i % 3)
                    cGrid((row * 9) + col)
                }
                cells.map(cell => Mux(isOneHot(cell), cell, 0.U)).reduce(_|_)
            })
        )

        // initializing candidate masks
        // for (row <- 0 until 9) {
        //     for (col <- 0 until 9) {
        //         val cell = grid((row*9) + col)
        //         when (isOneHot(cell)){           // only want to OR singles
        //             // Row
        //             nextRowMask(row) := nextRowMask(row) | cell
        //             // Col
        //             nextColMask(col) := nextColMask(col) | cell
        //             // Sub
        //             val boxInd = ((row / 3)) * 3 + (col / 3)
        //             nextSubMask(boxInd) := nextSubMask(boxInd) | cell
        //         }
        //     }
        // }

        rowMask := nextRowMask
        colMask := nextColMask
        subMask := nextSubMask
    }

    // find singles
    def pruneSingles() = {
        // first set candidates
        setCandidates(grid)

        // prune mask by or'ing row, col, and sub together
        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val boxInd = ((row / 3)) * 3 + (col / 3)
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
        for (r <- 0 until 27; i <- 0 until 9) {
            unsolvedCount(r)(i) := 0.U
        }

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
                val sub = ((row / 3) * 3) + (col / 3)
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
        val sub = ((row / 3.U) * 3.U) + (col / 3.U)

        // see if candidate is already in row, col, or sub
        !( (rowMask(row) & candOH).orR ||
           (colMask(col) & candOH).orR ||
           (subMask(sub) & candOH).orR )
    }

    def dfs() = {
        printf(p"[DFS] state=$state | cellIdx=$cellIdx | cell=0x${Hexadecimal(cell)}\n")
        printf(p"[DEBUG] empty=$empty valid=$valid candidate=$candidate traversed=$traversed cand=${candPtr(cellIdx)} candOH=0x${Hexadecimal(candOH)}\n")

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
            is (dfsState.findEmpty) {
                when (empty) {
                    // found empty cell, see if valid
                    candPtr(cellIdx) := 0.U
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
                    nextGrid(cellIdx) := candOH
                    stack(sp) := cellIdx
                    sp := sp + 1.U  
                    
                    setCandidates(nextGrid)

                    // candPtr(cellIdx) := 0.U

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

                    // rebuild masks after removing that guess
                    setCandidates(nextGrid) 

                    candPtr(prev) := candPtr(prev) + 1.U

                    cellIdx := prev
                    when (candidate) { 
                        state := dfsState.checkValid
                        setCandidates(nextGrid)
                    } .otherwise {// if already guessing 9th candidate
                        state := dfsState.backtrack
                    }
                }
            }
            is (dfsState.done) {
                dfsDone := true.B
            }
        }
    }

    switch(io.mode) {
        is(0.U) {pruneSingles()}
        is(1.U) {getHiddenSingles()}
        is(2.U) {dfs()}
    }
    io.done := dfsDone
    io.outGrid := nextGrid
}