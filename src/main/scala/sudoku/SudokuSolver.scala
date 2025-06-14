package sudoku

import chisel3._
import chisel3.util._

class SudokuSolver(gridsize: Int = 9) extends Module {
    val io = IO(new Bundle {
        val inGrid  = Input(Vec(gridsize*gridsize, UInt(gridsize.W)))
        val start   = Input(Bool())
        val done    = Output(Bool())
        val cycles  = Output(UInt(32.W))
        val outGrid = Output(Vec(gridsize*gridsize, UInt(gridsize.W)))
    })

    // WIRES
    val changed    = Wire(Bool())
    val solved     = Wire(Bool())
    val loadGrid   = Wire(Bool())
    val done       = Wire(Bool())
    val mode       = Wire(UInt(2.W))
    val cycleCount = Wire(UInt(32.W))

    // STORAGE
    val grid     = RegInit(VecInit(Seq.fill(gridsize*gridsize)(0.U(gridsize.W))))
    val nextGrid = WireInit(VecInit(Seq.fill(gridsize*gridsize)(0.U(gridsize.W))))

    // CONTROLLER
    val controller = Module(new SudokuController())
    controller.io.start      := io.start
    controller.io.changed    := changed
    controller.io.solved     := solved
    loadGrid                 := controller.io.loadGrid
    done                     := controller.io.done
    mode                     := controller.io.mode
    cycleCount               := controller.io.cycleCount

    // PROCESSOR
    val processor = Module(new SudokuProcessor(gridsize))
    processor.io.inGrid  := grid
    processor.io.mode    := mode
    changed              := processor.io.changed
    solved               := processor.io.done
    nextGrid             := processor.io.outGrid

    // OUTPUTS
    io.done := done
    io. cycles := cycleCount
    
    grid := Mux(loadGrid, io.inGrid, nextGrid)
    io.outGrid := grid
}