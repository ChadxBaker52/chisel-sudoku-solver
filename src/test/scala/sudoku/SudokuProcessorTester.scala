package sudoku

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import sudoku.SudokuProcessorModel

class SudokuProcessorTester extends AnyFlatSpec with ChiselScalatestTester {
  def oneHotToDigit(oneHot: BigInt): Int = {
    if (oneHot == 0x1FF) 0 // all 9 candidates still possible
    else if ((oneHot & (oneHot - 1)) == 0 && oneHot != 0) {
      // only one bit is set (true one-hot)
      oneHot.bitLength
    } else {
      0 // not solved yet — multiple candidates
    }
  }

  def isOneHot(cell: UInt): Boolean = {
    val value = cell.peek().litValue
    (value != 0) && ((value & (value - 1)) == 0)
  }

  def printGrid(grid: Vec[UInt]) = {
    println("=== Output Sudoku Grid ===")
    for (i <- 0 until 81) {
      val out = grid(i).peek().litValue
      val digit = oneHotToDigit(out)
      print(s"$digit ")
      if ((i + 1) % 9 == 0) println()
      // dut.io.outGrid(i).expect(correct(i))
    }
  }

  def setPuzzleCell(row: Int, col: Int, digit: Int, puzzle: Array[UInt]): Array[UInt] = {
    val idx = row * 9 + col
    if (digit >= 1 && digit <= 9)
      puzzle(idx) = (1 << (digit - 1)).U(9.W) // one-hot
    else if (digit == 0)
      puzzle(idx) = "b111111111".U(9.W) // means unknown
    else
      throw new IllegalArgumentException("Digit must be between 0 and 9")
    puzzle
  }

  behavior of "SudokuProcessor" 
  // it should "test dfs on empty grid" in {
  //   test(new SudokuProcessor()) { dut =>
  //     dut.clock.setTimeout(0)
  //     val puzzle  = Array.fill(81)("b111111111".U(9.W))

  //     for (i <- 0 until 81) dut.io.inGrid(i).poke(puzzle(i))

  //     dut.io.mode.poke(0.U)
  //     dut.clock.step(1)

  //     dut.io.mode.poke(1.U)
  //     dut.clock.step(1)

  //     dut.io.mode.poke(2.U)
  //     dut.clock.step(1)

  //     dut.io.mode.poke(3.U)
  //     var cycles = 0
  //     val maxCycles = 10000

  //     while (!dut.io.done.peek().litToBoolean && cycles < maxCycles) {
  //       dut.clock.step(1)
  //       cycles += 1
  //     }

  //     if (cycles >= maxCycles) {
  //       printGrid(dut.io.outGrid)
  //       fail("Solver did not complete in time")
  //     } else {
  //       println(s"Solved in $cycles cycles")
  //     }
  //     printGrid(dut.io.outGrid)
  //   }
  // }

  it should "test singles" in {
    test(new SudokuProcessor()) { dut => 
      dut.clock.setTimeout(0)

      val puzzle = Array(
        5,3,0, 0,7,0, 0,0,0,
        6,0,0, 1,9,5, 0,0,0,
        0,9,8, 0,0,0, 0,6,0,
        8,0,0, 0,6,0, 0,0,3,
        4,0,0, 8,0,3, 0,0,1,
        7,0,0, 0,2,0, 0,0,6,
        0,6,0, 0,0,0, 2,8,0,
        0,0,0, 4,1,9, 0,0,5,
        0,0,0, 0,8,0, 0,7,9
      )

      println("=== Input Scala Grid ===")
      for (i <- 0 until 81) {
        val digit = puzzle(i)
        print(s"$digit ")
        if ((i + 1) % 9 == 0) println()
      }

      val chiselPuzzle = puzzle.map {
        case d if d >= 1 && d <= 9 => (1 << (d - 1)).U(9.W)
        case 0 => "b111111111".U(9.W)
      }

      // Poke input puzzle
      for (i <- 0 until 81) {
        dut.io.inGrid(i).poke(chiselPuzzle(i))
      }

      dut.io.mode.poke(1.U)
      dut.clock.step(2)   // one cycle to get initialized, another to run singles

      val expected = SudokuProcessorModel.solveSingles(puzzle)

      printGrid(dut.io.outGrid)

      println("=== Output Scala Grid ===")
      for (i <- 0 until 81) {
        val digit = expected(i)
        print(s"$digit ")
        if ((i + 1) % 9 == 0) println()
      }

      for (i <- 0 until 81) {
        val cell = dut.io.outGrid(i)
        val actual = cell.peek().litValue
        val expectedDigit = expected(i)

        if (expectedDigit != 0 && isOneHot(cell)) {
          val expectedOneHot = BigInt(1) << (expectedDigit - 1)
          assert(actual == expectedOneHot,
            s"Mismatch at cell $i: expected one-hot $expectedOneHot, got $actual")
        }
      }
    }
  }

  it should "test hidden singles" in {
    test(new SudokuProcessor()) { dut => 
      dut.clock.setTimeout(0)

      dut.clock.setTimeout(0)

      val puzzle = Array(
        5,3,0, 0,7,0, 0,0,0,
        6,0,0, 1,9,5, 0,0,0,
        0,9,8, 0,0,0, 0,6,0,
        8,0,0, 0,6,0, 0,0,3,
        4,0,0, 8,0,3, 0,0,1,
        7,0,0, 0,2,0, 0,0,6,
        0,6,0, 0,0,0, 2,8,0,
        0,0,0, 4,1,9, 0,0,5,
        0,0,0, 0,8,0, 0,7,9
      )

      println("=== Input Scala Grid ===")
      for (i <- 0 until 81) {
        val digit = puzzle(i)
        print(s"$digit ")
        if ((i + 1) % 9 == 0) println()
      }

      val chiselPuzzle = puzzle.map {
        case d if d >= 1 && d <= 9 => (1 << (d - 1)).U(9.W)
        case 0 => "b111111111".U(9.W)
      }

      // Poke input puzzle
      for (i <- 0 until 81) {
        dut.io.inGrid(i).poke(chiselPuzzle(i))
      }

      dut.io.mode.poke(1.U)
      dut.clock.step(2)   // one cycle to get initialized, another to run singles

      val expected = SudokuProcessorModel.solveHiddenSingles(puzzle)

      printGrid(dut.io.outGrid)

      println("=== Output Scala Grid ===")
      for (i <- 0 until 81) {
        val digit = expected(i)
        print(s"$digit ")
        if ((i + 1) % 9 == 0) println()
      }

      for (i <- 0 until 81) {
        val cell = dut.io.outGrid(i)
        val actual = cell.peek().litValue
        val expectedDigit = expected(i)

        if (expectedDigit != 0 && isOneHot(cell)) {
          val expectedOneHot = BigInt(1) << (expectedDigit - 1)
          assert(actual == expectedOneHot,
            s"Mismatch at cell $i: expected one-hot $expectedOneHot, got $actual")
        }
      }

    }
  }
}