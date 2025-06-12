package sudoku

import chisel3._
import chisel3.util._
import SudokuUtils._

class SudokuProcessor(gridsize: Int) extends Module {
    val io = IO(new Bundle {
        val inGrid = Input(Vec(gridsize*gridsize, UInt(gridsize.W)))
        val mode = Input(UInt(2.W))
        val changed = Output(Bool())
        val done = Output(Bool())
        val outGrid = Output(Vec(gridsize*gridsize, UInt(gridsize.W)))
    })
    // input grid
    val grid = io.inGrid

    // Wire for nextGrid from singles
    val nextSinglesGrid = WireInit(VecInit(Seq.fill(gridsize*gridsize)(0.U(gridsize.W))))
    nextSinglesGrid := io.inGrid

    // Wire for nextGrid from DFS
    val nextDFSGrid = WireInit(VecInit(Seq.fill(gridsize*gridsize)(0.U(gridsize.W))))
    nextDFSGrid := io.inGrid

    // Wire for nextGrid
    val nextGrid = WireInit(VecInit(Seq.fill(gridsize*gridsize)(0.U(gridsize.W))))

    // Ref grid for dfs
    val refGrid = RegInit(VecInit(Seq.fill(gridsize*gridsize)(0.U(gridsize.W))))

    // final grid
    val finalGrid = RegInit(VecInit(Seq.fill(gridsize*gridsize)(0.U(gridsize.W))))

    // 27 9-bit vectors for Candidates
    val rowMask = RegInit(VecInit(Seq.fill(gridsize)(0.U(gridsize.W))))
    val colMask = RegInit(VecInit(Seq.fill(gridsize)(0.U(gridsize.W))))
    val subMask = RegInit(VecInit(Seq.fill(gridsize)(0.U(gridsize.W))))

    val changed = Wire(Bool())
    changed := false.B

    // Scala Constants
    val N = math.sqrt(gridsize).toInt

    // find singles
    def pruneSingles() = {
        // new 27 9-bit vectors for Candidates
        val nextRowMask = WireInit(
            VecInit((0 until gridsize).map { row =>
                val rowCells = (0 until gridsize).map(col => grid(row*gridsize + col))
                rowCells.map(cell => Mux(isOneHot(cell), cell, 0.U)).reduce(_|_)
            })
        )
        val nextColMask = WireInit(
            VecInit((0 until gridsize).map { col =>
                val colCells = (0 until gridsize).map(row => grid(row*gridsize + col))
                colCells.map(cell => Mux(isOneHot(cell), cell, 0.U)).reduce(_|_)
            })
        )
        val nextSubMask = WireInit(
            VecInit((0 until gridsize).map { sub =>
                val cells = (0 until gridsize).map { i =>
                    val row = (sub / N) * N + (i / N)
                    val col = (sub % N) * N + (i % N)
                    grid((row * gridsize) + col)
                }
                cells.map(cell => Mux(isOneHot(cell), cell, 0.U)).reduce(_|_)
            })
        )

        // prune mask by or'ing row, col, and sub together
        for (row <- 0 until gridsize) {
            for (col <- 0 until gridsize) {
                val boxInd = ((row / N)) * N + (col / N)
                val cell = grid((row*gridsize) + col)
                val finalMask = ~(nextRowMask(row) | nextColMask(col) | nextSubMask(boxInd))
                val prunedMask = cell & finalMask

                val isFinal = isOneHot(cell)
                val nextVal = Mux(isFinal, cell, prunedMask)

                nextSinglesGrid((row*gridsize) + col) := nextVal

                when (!isFinal && cell =/= prunedMask) {
                    changed := true.B
                } 
            }
        }

        rowMask := nextRowMask
        colMask := nextColMask
        subMask := nextSubMask
        
        refGrid := nextSinglesGrid
    }
    
    // DFS Variables
    val cellIdx = RegInit(0.U(log2Ceil(gridsize*gridsize).W))
    val cell = nextDFSGrid(cellIdx)
    val candPtr = RegInit(VecInit(Seq.fill(gridsize*gridsize)(0.U(log2Ceil(gridsize).W))))
    val cellStack = Reg(Vec(gridsize*gridsize, UInt(log2Ceil(gridsize*gridsize).W)))
    val sp = RegInit(0.U(log2Ceil(gridsize*gridsize).W))
    val dfsDone = RegInit(false.B)
    val cand = candPtr(cellIdx)

    val empty = ~isOneHot(cell)
    val candOH = UIntToOH(cand, gridsize)
    val valid = isValidCell(cellIdx, candOH)
    val hasCand = candPtr(cellIdx) < gridsize.U
    val traversed = cellIdx === (gridsize*gridsize).U

    def isValidCell(idx: UInt, candOH: UInt): Bool = {
        val row = idx / gridsize.U
        val col = idx % gridsize.U
        val sub = ((row / N.U) * N.U) + (col / N.U)

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
        // Go to first cell
        switch (state) {
            is (dfsState.idle) {
                // initialize everything
                cellIdx := 0.U
                sp := 0.U
                for (i <- 0 until (gridsize*gridsize)) candPtr(i) := 0.U

                // see if cell is empty
                state   := dfsState.findEmpty
            }
            is (dfsState.findEmpty) {
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
                    nextDFSGrid(cellIdx) := candOH
                    cellStack(sp) := cellIdx
                    sp := sp + 1.U  
                    
                    // adding to masks
                    val row = cellIdx / gridsize.U
                    val col = cellIdx % gridsize.U
                    val sub = (row/N.U) * N.U + (col/N.U)
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
                nextDFSGrid(prev) := refGrid(prev)

                // rebuild masks after removing that guess
                // removing from masks
                val row = cellIdx / gridsize.U
                val col = cellIdx % gridsize.U
                val sub = (row/N.U) * N.U + (col/N.U)
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
                finalGrid := nextDFSGrid
                dfsDone := true.B
            }
        }
    }

    switch(io.mode) {
        is(1.U) {
            pruneSingles()
            nextGrid := nextSinglesGrid
        }
        is(2.U) {
            dfs()
            nextGrid := nextDFSGrid
        }
    }

    io.changed := changed
    io.done := dfsDone
    io.outGrid := Mux(dfsDone, finalGrid, nextGrid)
}