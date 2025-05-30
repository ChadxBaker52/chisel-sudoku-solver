package sudoku

object SudokuProcessorModel {
    def solveSingles(grid: Array[Int]): Array[Int] = {
        require(grid.length == 81, "Grid must be 81 elements")

        def row(i: Int): Int = i / 9
        def col(i: Int): Int = i % 9
        def subgrid(i: Int): Int = (row(i) / 3) * 3 + (col(i) / 3)

        def valuesInRow(r: Int): Set[Int] =
            (0 until 9).map(c => grid(r * 9 + c)).filter(_ != 0).toSet

        def valuesInCol(c: Int): Set[Int] =
            (0 until 9).map(r => grid(r * 9 + c)).filter(_ != 0).toSet

        def valuesInSubgrid(s: Int): Set[Int] = {
            val topLeftRow = (s / 3) * 3
            val topLeftCol = (s % 3) * 3
            (0 until 3).flatMap { r =>
                (0 until 3).map { c =>
                    grid((topLeftRow + r) * 9 + (topLeftCol + c))
                }
            }.filter(_ != 0).toSet
        }

        val updated = grid.clone()

        for (i <- grid.indices if grid(i) == 0) {
            val r = row(i)
            val c = col(i)
            val s = subgrid(i)

            val used = valuesInRow(r) ++ valuesInCol(c) ++ valuesInSubgrid(s)
            val options = (1 to 9).toSet -- used

            if (options.size == 1) {
                updated(i) = options.head
            }
        }

        updated
    }

    def solveHiddenSingles(grid: Array[Int]): Array[Int] = {
        require(grid.length == 81, "Grid must be 81 elements")
        val updated = grid.clone()

        def row(i: Int): Int = i / 9
        def col(i: Int): Int = i % 9
        def subgrid(i: Int): Int = (row(i) / 3) * 3 + (col(i) / 3)

        def unitIndicesRow(r: Int): Seq[Int] = (0 until 9).map(c => r * 9 + c)
        def unitIndicesCol(c: Int): Seq[Int] = (0 until 9).map(r => r * 9 + c)
        def unitIndicesSubgrid(s: Int): Seq[Int] = {
            val topLeftRow = (s / 3) * 3
            val topLeftCol = (s % 3) * 3
            for {
            r <- 0 until 3
            c <- 0 until 3
            } yield (topLeftRow + r) * 9 + (topLeftCol + c)
        }

        def possibleDigitsAt(i: Int): Set[Int] = {
            if (grid(i) != 0) Set.empty
            else {
            val r = row(i)
            val c = col(i)
            val s = subgrid(i)
            val used = (unitIndicesRow(r) ++ unitIndicesCol(c) ++ unitIndicesSubgrid(s))
                .distinct.map(grid).filter(_ != 0).toSet
                (1 to 9).toSet -- used
            }
        }

        def updateFromUnits(unitGetter: Int => Seq[Int]): Unit = {
            for (u <- 0 until 9) {
            val indices = unitGetter(u)
            val possibilities = indices.map(i => i -> possibleDigitsAt(i)).toMap
            for (digit <- 1 to 9) {
                val candidateCells = possibilities.collect {
                case (idx, opts) if opts.contains(digit) => idx
                }.toSeq
                if (candidateCells.size == 1) {
                val idx = candidateCells.head
                updated(idx) = digit
                }
            }
            }
        }

        updateFromUnits(unitIndicesRow)
        updateFromUnits(unitIndicesCol)
        updateFromUnits(unitIndicesSubgrid)

        updated
    }


    def dfsSolveSudoku(grid: Array[Int]): Option[Array[Int]] = {
        require(grid.length == 81, "Grid must be 81 elements")

        def isValid(grid: Array[Int], pos: Int, value: Int): Boolean = {
            val r = pos / 9
            val c = pos % 9
            val sRow = (r / 3) * 3
            val sCol = (c / 3) * 3

            // Check row
            for (i <- 0 until 9 if grid(r * 9 + i) == value) return false
            // Check column
            for (i <- 0 until 9 if grid(i * 9 + c) == value) return false
            // Check subgrid
            for (i <- 0 until 3; j <- 0 until 3) {
                if (grid((sRow + i) * 9 + (sCol + j)) == value) return false
            }

            true
        }

        def dfs(grid: Array[Int], index: Int): Boolean = {
            if (index == 81) return true

            if (grid(index) != 0) return dfs(grid, index + 1)

            for (value <- 1 to 9) {
            if (isValid(grid, index, value)) {
                grid(index) = value
                if (dfs(grid, index + 1)) return true
                grid(index) = 0
            }
            }

            false
        }

        val copied = grid.clone()
        if (dfs(copied, 0)) Some(copied) else None
    }
}