package sudoku

import chisel3._
import chisel3.util._

class SudokuController() extends Module {
    val io = IO(new Bundle {
        val start = Input(Bool())
        val changed = Input(Bool())
        val solved = Input(Bool())
        val loadGrid = Output(Bool())
        val done = Output(Bool())
        val mode = Output(UInt(2.W))
        val cycleCount = Output(UInt(32.W))
    })
    val grid = RegInit(VecInit(Seq.fill(9*9)(0.U(9.W))))

    object pState extends ChiselEnum {
        val Idle, Single, Hidden, DFS, Done = Value
    }
    val state = RegInit(pState.Idle)

    val done = RegInit(false.B)
    val nextMode = RegInit(0.U(2.W))
    val startCount = (state === pState.Single) || (state === pState.Hidden) || (state === pState.DFS)
    val (cycles, _) = Counter(startCount, 16777216) // allows for 10 mil cycles
    val load = state === pState.Idle

    switch (state) {
        is (pState.Idle) {
            nextMode := 0.U
            when (io.start) {
                nextMode := 1.U
                state := pState.Single
            }
        }
        is (pState.Single) {
            nextMode := 1.U
            when (io.solved) {
                nextMode := 0.U
                state := pState.Done
            } .elsewhen (~io.changed) { // & ~solved
                nextMode := 2.U
                state := pState.DFS
            }
        }
        is (pState.DFS) {
            when (io.solved) {
                nextMode := 0.U
                done := true.B
                state := pState.Done
            }
            
            nextMode := 2.U
        }
        is (pState.Done) {
            nextMode := 0.U
            done := true.B
        }
    }

    
    io.loadGrid := load
    io.done := done
    io.mode := nextMode
    io.cycleCount := cycles
}