# Chisel Sudoku Solver

## Overview
This repository contains the beginnings of a parameterized Sudoku solver implemented in Chisel. The goal is to implement a hardware approach towards solving a sudoku puzzle.

### Completed Components
- **SudokuGrid**: A `Bundle` defining the Sudoku grid as a `Vec` of 81 `UInt(9.W)` cells, with helpers to set/reset and index values.
- **SudokuProcessor**: A `Module` that implements the core solving passes (candidates + singles, hidden-singles) and exposes `io.inGrid`, `io.outGrid`, and flags (`changed`, `done`) for each pass.
- **SudokuController**: A top-level `Module` containing an FSM that sequences the processor’s modes, counts cycles, and produces the final `done` signal.

## Next Steps
1. **Scala Sudoku Model**: Implement a pure-Scala backtracking Sudoku solver for reference (`SudokuModel`) and add unit tests against known puzzles.  
2. **Integration**: Wire the `SudokuProcessor` under control of `SudokuController`.  
3. **Testing**: Develop comprehensive `ChiselTest` for each module, testing functionality by knowing correct output.

## Build & Test
**Prerequisites:** Java 11+, SBT

```bash
# Compile the project
sbt compile

# Run all tests (Scala model + Chisel tests)
sbt test
