package sudoku

import chisel3._
import chisel3.util._
import SudokuUtils._

class SudokuProcessor() extends Module {
    val io = IO(new Bundle {
        val inGrid = Input(Vec(9*9, UInt(9.W)))
        val mode = Input(UInt(2.W))
        val changed = Output(Bool())
        val done = Output(Bool())
        val outGrid = Output(Vec(9*9, UInt(9.W)))
    })
    // 
    val grid = io.inGrid
    // make a wire
    val nextGrid = RegInit(VecInit(Seq.fill(9*9)(0.U(9.W))))
    // main grid
    val refGrid = RegInit(VecInit(Seq.fill(9*9)(0.U(9.W))))
    nextGrid := io.inGrid

    // 27 9-bit vectors for Candidates
    val rowMask = RegInit(VecInit(Seq.fill(9)(0.U(9.W))))
    val colMask = RegInit(VecInit(Seq.fill(9)(0.U(9.W))))
    val subMask = RegInit(VecInit(Seq.fill(9)(0.U(9.W))))

    val changed = Wire(Bool())
    changed := false.B

    // find singles
    def pruneSingles() = {
        // new 27 9-bit vectors for Candidates
        val nextRowMask = WireInit(
            VecInit((0 until 9).map { row =>
                val rowCells = (0 until 9).map(col => grid(row*9 + col))
                rowCells.map(cell => Mux(isOneHot(cell), cell, 0.U)).reduce(_|_)
            })
        )
        val nextColMask = WireInit(
            VecInit((0 until 9).map { col =>
                val colCells = (0 until 9).map(row => grid(row*9 + col))
                colCells.map(cell => Mux(isOneHot(cell), cell, 0.U)).reduce(_|_)
            })
        )
        val nextSubMask = WireInit(
            VecInit((0 until 9).map { sub =>
                val cells = (0 until 9).map { i =>
                    val row = (sub / 3) * 3 + (i / 3)
                    val col = (sub % 3) * 3 + (i % 3)
                    grid((row * 9) + col)
                }
                cells.map(cell => Mux(isOneHot(cell), cell, 0.U)).reduce(_|_)
            })
        )

        // prune mask by or'ing row, col, and sub together
        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val boxInd = ((row / 3)) * 3 + (col / 3)
                val cell = grid((row*9) + col)
                val finalMask = ~(nextRowMask(row) | nextColMask(col) | nextSubMask(boxInd))
                val prunedMask = cell & finalMask

                val isFinal = isOneHot(cell)
                val nextVal = Mux(isFinal, cell, prunedMask)

                nextGrid((row*9) + col) := nextVal

                when (!isFinal && cell =/= prunedMask) {
                changed := true.B
                printf(p"[row=$row col=$col] cell=0b${Binary(cell)}  mask=0b${Binary(prunedMask)}\n")   
                }
            }
        }

        rowMask := nextRowMask
        colMask := nextColMask
        subMask := nextSubMask
        
        // changed := nextGrid.zip(refGrid).map { case (a, b) => a =/= b }.reduce(_ || _)
        refGrid := nextGrid
    }


    // 27 Vectors full of 9 4-bit vectors used for Hidden Singles
    val unsolvedCount = RegInit(VecInit(Seq.fill(27)(VecInit(Seq.fill(9)(0.U(4.W))))))

    // Counting cells in each row, col, and sub
    for (row <- 0 until 9) {
        for (col <- 0 until 9) {
            val cell = grid(row * 9 + col).asBools
            val sub = (row/3)*3 + col/3
            for (i <- 0 until 9) {
                when (cell(i)) {
                    unsolvedCount(row)(i) := unsolvedCount(row)(i) + 1.U
                    unsolvedCount(col+9)(i) := unsolvedCount(col+9)(i) + 1.U
                    unsolvedCount(sub+18)(i) := unsolvedCount(sub+18)(i) + 1.U
                }
            }
        }
    }

    def getHiddenSingles() = {
        // Create hidden masks
        val hRowMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))
        val hColMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))
        val hSubMask = WireInit(VecInit(Seq.fill(9)(0.U(9.W))))

        // Counting cells
        // ----------------------------------------------------
        // Counting cells in each row, col, and sub
        for (row <- 0 until 9) {
            for (col <- 0 until 9) {
                val cell = grid(row * 9 + col).asBools
                val sub = (row/3)*3 + col/3
                for (i <- 0 until 9) {
                    when (cell(i)) {
                        unsolvedCount(row)(i) := unsolvedCount(row)(i) + 1.U
                        unsolvedCount(col+9)(i) := unsolvedCount(col+9)(i) + 1.U
                        unsolvedCount(sub+18)(i) := unsolvedCount(sub+18)(i) + 1.U
                    }
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
    
    // DFS Variables
    val cellIdx = RegInit(0.U(log2Ceil(81).W))
    val cell = nextGrid(cellIdx)
    val candPtr = RegInit(VecInit(Seq.fill(81)(0.U(4.W))))
    val cellStack = Reg(Vec(81, UInt(7.W)))
    val sp = RegInit(0.U(7.W))
    val dfsDone = RegInit(false.B)
    val cand = candPtr(cellIdx)
    // val candValid = cand > 0.U && cand <= 9.U
    // val safeCand = Mux(candValid, cand - 1.U, 0.U)

    val empty = ~isOneHot(cell)
    val candOH = UIntToOH(cand)(8, 0)
    val valid = isValidCell(cellIdx, candOH)
    val hasCand = candPtr(cellIdx) < 8.U
    val traversed = cellIdx === 81.U

    def isValidCell(idx: UInt, candOH: UInt): Bool = {
        val row = idx / 9.U
        val col = idx % 9.U
        val sub = ((row / 3.U) * 3.U) + (col / 3.U)

        // see if candidate is already in row, col, or sub
        !( (rowMask(row) & candOH).orR ||
           (colMask(col) & candOH).orR ||
           (subMask(sub) & candOH).orR )
    }
    
    object dfsState extends ChiselEnum {
        val idle, findEmpty, checkValid, backtrack, done = Value
    }
    val state = RegInit(dfsState.idle)

    def dfs() = {
        // printf(p"[DFS] state=$state | cellIdx=$cellIdx | cell=0b${cell}\n")
        // printf(p"[DEBUG] empty=$empty valid=$valid hasCand=$hasCand traversed=$traversed cand=${candPtr(cellIdx)}\n\n")
        // printf("\n")

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
                // val row = cellIdx / 9.U
                // val col = cellIdx % 9.U
                // val sub = (row/3.U) * 3.U + (col/3.U)
                // printf("[MASKS]  ")
                // printf(p"rowMask=0b${Binary(rowMask(row))}  ")   
                // printf(p"colMask=0b${Binary(colMask(col))}  ")
                // printf(p"subMask=0b${Binary(subMask(sub))}\n")
                when (empty) {
                    // found empty cell
                    // start at first candidate
                    candPtr(cellIdx) := 0.U
                    // see if valid
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
                    // printf(p"[CELL] cellIdx=0x${cellIdx} set to cand=0x${cand+1.U}\n")
                    nextGrid(cellIdx) := candOH
                    cellStack(sp) := cellIdx
                    sp := sp + 1.U  
                    
                    // adding to masks
                    val row = cellIdx / 9.U
                    val col = cellIdx % 9.U
                    val sub = (row/3.U) * 3.U + (col/3.U)
                    rowMask(row) := rowMask(row) | candOH
                    colMask(col) := colMask(col) | candOH
                    subMask(sub) := subMask(sub) | candOH

                    // go to next cell
                    cellIdx := cellIdx + 1.U

                    // see if next cell is empty
                    state := dfsState.findEmpty
                } .elsewhen (hasCand) {
                    // go to next candidate
                    candPtr(cellIdx) := candPtr(cellIdx) + 1.U
                    // see if valid
                    state := dfsState.checkValid
                } .otherwise {
                    // out of candidates
                    // pop off of stack
                    cellIdx := cellStack(sp - 1.U)
                    // move pointer back
                    sp := sp - 1.U
                    state := dfsState.backtrack
                }
            }
            is (dfsState.backtrack) {
                // get previous cell
                val prev = cellStack(sp)
                // restore old val
                nextGrid(prev) := refGrid(prev)

                // rebuild masks after removing that guess
                // setCandidates(nextGrid) 
                // removing from masks
                val row = cellIdx / 9.U
                val col = cellIdx % 9.U
                val sub = (row/3.U) * 3.U + (col/3.U)
                rowMask(row) := rowMask(row) & ~candOH
                colMask(col) := colMask(col) & ~candOH
                subMask(sub) := subMask(sub) & ~candOH

                when (hasCand) { 
                    candPtr(prev) := candPtr(prev) + 1.U
                    state := dfsState.checkValid
                } .otherwise {// if already guessing 9th candidate
                    // pop off of stack
                    cellIdx := cellStack(sp - 1.U)
                    // move pointer back
                    sp := sp - 1.U
                    state := dfsState.backtrack
                }
            }
            is (dfsState.done) {
                dfsDone := true.B
            }
        }
    }

    switch(io.mode) {
        is(1.U) {pruneSingles()}
        // is(2.U) {getHiddenSingles()}
        is(2.U) {dfs()}
    }

    io.changed := changed
    io.done := dfsDone
    io.outGrid := nextGrid
}