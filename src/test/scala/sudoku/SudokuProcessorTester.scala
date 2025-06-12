package sudoku

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import sudoku.SudokuTestUtils.chooseBackend

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

  def printGrid(grid: Vec[UInt], gridsize: Int = 9) = {
    println("=== Output Sudoku Grid ===")
    for (i <- 0 until gridsize*gridsize) {
      val out = grid(i).peek().litValue
      val digit = oneHotToDigit(out)
      print(s"$digit ")
      if ((i + 1) % gridsize == 0) println()
    }
  }

  def doProcessorTest(dut: SudokuProcessor, puzzle: Array[Int], inGrid: Array[UInt], mode: Int, gridsize: Int = 9): (Array[Int], Array[UInt], Int, Boolean) = {
    for (i <- 0 until gridsize*gridsize) {
      dut.io.inGrid(i).poke(inGrid(i))
    }

    dut.io.mode.poke(mode.U)
    dut.clock.step()

    // val expectedOut: Array[Int] = mode match {
    //   case 1 => SudokuProcessorModel.solveSingles(puzzle)
    //   // case 2 => SudokuProcessorModel.solveHiddenSingles(puzzle)
    //   case 2 => SudokuProcessorModel.dfsSolveSudoku(puzzle) match {
    //     case Some(solution) => solution
    //     case None => throw new RuntimeException("DFS failed to solve puzzle")
    //   }
    //   case _ => throw new IllegalArgumentException(s"Unknown mode $mode")
    // }

    val outPuzzle: Array[Int] = (0 until gridsize*gridsize).map { i =>
      val value = dut.io.outGrid(i).peek().litValue
      oneHotToDigit(value)
    }.toArray

    val expectedOut: Array[Int] = (0 until gridsize*gridsize).map { i =>
      val value = dut.io.outGrid(i).peek().litValue
      oneHotToDigit(value)
    }.toArray

    val outGrid: Array[UInt] = (0 until gridsize*gridsize).map(i => dut.io.outGrid(i).peek()).toArray

    val changed: Boolean = dut.io.changed.peek().litToBoolean

    printGrid(dut.io.outGrid, gridsize)

    val nextMode = if (!changed & (mode == 1)) mode + 1 else mode
    (expectedOut, outGrid, nextMode, changed)
  }

  it should "test singles" in {
    test(new SudokuProcessor(9))
      .withAnnotations(chooseBackend())
      .apply{ dut => 
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

  it should "test dfs on empty grid" in {
    test(new SudokuProcessor(4))
    .withAnnotations(chooseBackend())
    .apply { dut =>
      dut.clock.setTimeout(0)

      // Initial empty puzzle (all candidates)
      val puzzle = Array.fill(16)(0)
      val chiselPuzzle = puzzle.map {
        case d if d >= 1 && d <= 4 => (1 << (d - 1)).U(4.W)
        case 0 => "b1111".U(4.W)
      }

      var currentPuzzle = puzzle
      var currentGrid = chiselPuzzle
      var mode = 1
      val maxMode = 2
      val maxTries = 100
      var tries = 0

      while (!dut.io.done.peek().litToBoolean && tries < maxTries) {
        // println(s"\n----- cycle $tries -----\n")
        val (nextPuzzle, nextGrid, nextMode, changed) = doProcessorTest(dut, currentPuzzle, currentGrid, mode, 4)
        // println(s"\nmode: $mode    changed: $changed\n")
        // Update state for next iteration
        currentPuzzle = nextPuzzle
        currentGrid = nextGrid
        mode = nextMode
        tries += 1
      }

      // Final assertion — outGrid must match currentPuzzle
      for (i <- 0 until 16) {
        val actual = oneHotToDigit(dut.io.outGrid(i).peek().litValue)
        val expected = currentPuzzle(i)
        if (expected != 0) {
          assert(actual == expected, s"Mismatch at cell $i: expected $expected, got $actual")
        }
      }
    }
  }

  it should "test singles and dfs" in {
    test(new SudokuProcessor(9))
      .withAnnotations(chooseBackend())
      .apply{ dut =>
      dut.clock.setTimeout(0)

      val puzzle = Array(
        4,0,0, 0,7,8, 6,0,3,
        0,5,1, 3,9,0, 0,0,0,
        0,6,0, 4,2,0, 0,0,0,

        0,0,8, 0,0,0, 0,0,0,
        9,0,2, 0,5,0, 3,0,0,
        0,0,5, 6,0,0, 0,0,1,

        0,0,0, 5,0,0, 0,0,6,
        0,7,0, 2,0,0, 1,0,4,
        0,0,3, 0,0,0, 0,0,7
      )

      val chiselPuzzle = puzzle.map {
        case d if d >= 1 && d <= 9 => (1 << (d - 1)).U(9.W)
        case 0 => "b111111111".U(9.W)
      }

      var currentPuzzle = puzzle
      var currentGrid = chiselPuzzle
      var mode = 1
      val maxMode = 2
      val maxTries = 100
      var tries = 0

      while (!dut.io.done.peek().litToBoolean && tries < maxTries) {
        println(s"\n----- cycle $tries -----\n")
        val (nextPuzzle, nextGrid, nextMode, changed) = doProcessorTest(dut, currentPuzzle, currentGrid, mode)
        println(s"\nmode: $mode    changed: $changed\n")
        currentPuzzle = nextPuzzle
        currentGrid = nextGrid
        mode = nextMode
        tries += 1
      }

      // Final assertion
      for (i <- 0 until 81) {
        val actual = oneHotToDigit(dut.io.outGrid(i).peek().litValue)
        val expected = currentPuzzle(i)
        if (expected != 0) {
          assert(actual == expected, s"Mismatch at cell $i: expected $expected, got $actual")
        }
      }
    }
  }
}


  // it should "test hidden singles" in {
  //   test(new SudokuProcessor()) { dut => 
  //     dut.clock.setTimeout(0)

  //     val puzzle = Array(
  //       5,3,0, 0,7,0, 0,0,0,
  //       6,0,0, 1,9,5, 0,0,0,
  //       0,9,8, 0,0,0, 0,6,0,
  //       8,0,0, 0,6,0, 0,0,3,
  //       4,0,0, 8,0,3, 0,0,1,
  //       7,0,0, 0,2,0, 0,0,6,
  //       0,6,0, 0,0,0, 2,8,0,
  //       0,0,0, 4,1,9, 0,0,5,
  //       0,0,0, 0,8,0, 0,7,9
  //     )

  //     println("=== Input Scala Grid ===")
  //     for (i <- 0 until 81) {
  //       val digit = puzzle(i)
  //       print(s"$digit ")
  //       if ((i + 1) % 9 == 0) println()
  //     }

  //     val chiselPuzzle = puzzle.map {
  //       case d if d >= 1 && d <= 9 => (1 << (d - 1)).U(9.W)
  //       case 0 => "b111111111".U(9.W)
  //     }

  //     // Poke input puzzle
  //     for (i <- 0 until 81) {
  //       dut.io.inGrid(i).poke(chiselPuzzle(i))
  //     }

  //     dut.io.mode.poke(1.U)
  //     dut.clock.step(2)   // one cycle to get initialized, another to run singles

  //     val expected = SudokuProcessorModel.solveHiddenSingles(puzzle)

  //     printGrid(dut.io.outGrid)

  //     println("=== Output Scala Grid ===")
  //     for (i <- 0 until 81) {
  //       val digit = expected(i)
  //       print(s"$digit ")
  //       if ((i + 1) % 9 == 0) println()
  //     }

  //     for (i <- 0 until 81) {
  //       val cell = dut.io.outGrid(i)
  //       val actual = cell.peek().litValue
  //       val expectedDigit = expected(i)

  //       if (expectedDigit != 0 && isOneHot(cell)) {
  //         val expectedOneHot = BigInt(1) << (expectedDigit - 1)
  //         assert(actual == expectedOneHot,
  //           s"Mismatch at cell $i: expected one-hot $expectedOneHot, got $actual")
  //       }
  //     }

  //   }
  // }
