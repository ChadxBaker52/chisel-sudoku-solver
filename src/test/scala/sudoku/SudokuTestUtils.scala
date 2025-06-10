package sudoku

import chiseltest._
import scala.sys.process._
import chiseltest.simulator.SimulatorAnnotation

object SudokuTestUtils {
  def chooseBackend(): Seq[SimulatorAnnotation] = {
    val haveVerilator = try {
      Process("verilator" :: "--version" :: Nil).!(ProcessLogger(_ => ())) == 0
    } catch { case _: Throwable => false }

    if (haveVerilator) {
      println("[INFO] Found Verilator on PATH, using Verilator backend")
      Seq(VerilatorBackendAnnotation)
    } else {
      println("[INFO] Verilator not found, using Treadle backend")
      Seq(TreadleBackendAnnotation)
    }
  }
}
