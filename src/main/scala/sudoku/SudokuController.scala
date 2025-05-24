package sudoku

import chisel3._
import chisel3.util._

class SudokuController() extends Module {
    val io = IO(new Bundle {
        val start = Input(Bool())
        val changed = Input(Bool())
        val solved = Input(Bool())
        val inGrid = Input(Vec(9*9, UInt(9.W)))
        val loadGrid = Output(Bool())
        val done = Output(Bool())
        val mode = Output(UInt(2.W))
        val cycleCount = Output(UInt())
        val outGrid = Output(Vec(9*9, UInt(9.W)))
    })

    object pState extends ChiselEnum {
        val Idle, Single, Hidden, DFS, Done = Value
    }

    val state = RegInit(pState.Idle)
    
    val grid = WireInit(VecInit(Seq.fill(9*9)(0.U(9.W))))

    switch (state) {
        is (pState.Idle) {
            when (io.start) {
                state := pState.Single
            }
        }
        is (pState.Single) {
            when (io.solved) {
                state := pState.Done
            } .elsewhen (~io.changed & ~io.solved) {
                state := pState.Hidden
            }
        }
        is (pState.Hidden) {
            when (io.solved) {
                state := pState.Done
            } .elsewhen (io.changed) {
                state := pState.Single
            } .otherwise {
                state := pState.DFS
            }
        }
        is (pState.DFS) {
            when (io.solved) {
                state := pState.Done
            }
        }
        // is (pState.Done) {

        // }
    }
}