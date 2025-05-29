package sudoku

import chisel3._
import chisel3.util._

object SudokuUtils {
  def isOneHot(x: UInt): Bool = {
    x =/= 0.U && (x & (x - 1.U)) === 0.U
  }
}