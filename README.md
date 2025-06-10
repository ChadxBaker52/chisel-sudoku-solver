# Chisel Sudoku Solver

A hardware-accelerated Sudoku solver implemented in [Chisel](https://www.chisel-lang.org/).  
It performs classic solving techniques (naked & hidden singles) and DFS backtracking in synthesizable hardware.

---

## Overview

- **SudokuGrid**: Defines a 9×9 grid as `Vec(81, UInt(9.W))` with helper methods for one-hot encoding.
- **SudokuSolver** Top Module that connects SudokuController and SudokuProcessor
- **SudokuProcessor**: Core passes for
  - Candidate pruning (bitmask elimination)
  - Naked singles
  - Hidden singles (Work in Progress)
  - DFS backtracking
- **SudokuController**: FSM that drives `SudokuProcessor` through modes, counts cycles, and asserts `done`.
- **Test Utilities**: Automatic fallback between Verilator and Treadle backends.
- **Scala Model**: (WIP) Pure-Scala solver for reference and golden testing.

---

## Prerequisites

- **Java 11+**  
- **SBT**  
- **Chisel 3.6.x**  
- **(Optional, for speed) Verilator 4.x+**  

---

## Installation

1. **Clone the repo**
   ```bash
   git clone https://github.com/yourusername/chisel-sudoku-solver.git
   cd chisel-sudoku-solver

2. **Install Verilator (optional, speeds up tests)**
    ```bash
    sudo apt-get update
    sudo apt-get install -y verilator
    verilator --version

3. **Build & Test**

>If Verilator is not installed, tests will automatically fall back to Treadle (slower but works).

> **Important:** Many tests use a large iteration limit (`maxTries`) to allow the solver to complete.  
> You **must** increase this value in your test code to simulate most puzzles.


    sbt compile
To run all Test

    sbt test

To run individual test 

    sbt
    testOnly SudokuProcessorTester
    testOnly SudokuSolverTester
    testOnly SudokuControllerTester


## Test Details

- **Max Iterations (`maxTries`)**  
  In each test you will see a loop with:
  ```scala
  var maxTries = 100

You should increase this value for harder puzzles or when running on the slower Treadle backend.

### Puzzle Format
- Represented as Array[Int] of length 81.
- 0 = empty cell, 1–9 = given digit.

Use provided converter functions:

- intToOneHot(d: Int): UInt
- oneHotToInt(v: BigInt): Int

### Testing Workflow
1. Singles pass (mode = 1): Prunes candidates and fills naked/hidden singles.
2. DFS pass (mode = 2): Backtracking completes the grid.
3. Final assertion compares hardware io.outGrid to the Scala golden model.

### Backend Fallback
If Verilator is installed, tests use Verilator (much faster).

If Verilator is not found, tests automatically fall back to Treadle.


## Project Structure
    build.sbt                     — Project configuration.
    src/main/scala/sudoku/
        SudokuGrid.scala            — One-hot grid type & helpers.
        SudokuSolver.scala          — Top Module
        SudokuProcessor.scala       — Core solving passes.
        SudokuController.scala      — FSM to sequence solving passes and assert done.
    src/test/scala/sudoku/
        SudokuProcessorTester.scala — Unit tests for core solving passes.
        SudokuSolverTester.scala    — Full top-level solver tests.
        SudokuControllerTester.scala— Tests the FSM + controller flow.
        SudokuTestUtils.scala       — Helper functions for backend selection.