package com.fq.sudoku

import com.fq.sudoku.Solver.Value
import com.fq.sudoku.Solver.Coord._
import zio.test._

object UtilsTest extends ZIOSpecDefault {

  def spec =
    suite("UtilsTest")(
      test("boardToString") {
        assertTrue(Solver.toString(allCoords.map(Value.Given(_, 1))) ==
          """+-------+-------+-------+
            || 1 1 1 | 1 1 1 | 1 1 1 |
            || 1 1 1 | 1 1 1 | 1 1 1 |
            || 1 1 1 | 1 1 1 | 1 1 1 |
            |+-------+-------+-------+
            || 1 1 1 | 1 1 1 | 1 1 1 |
            || 1 1 1 | 1 1 1 | 1 1 1 |
            || 1 1 1 | 1 1 1 | 1 1 1 |
            |+-------+-------+-------+
            || 1 1 1 | 1 1 1 | 1 1 1 |
            || 1 1 1 | 1 1 1 | 1 1 1 |
            || 1 1 1 | 1 1 1 | 1 1 1 |
            |+-------+-------+-------+""".stripMargin)
      },
      test("boardToString can handle missing values") {
        assertTrue(Solver.toString(List.empty) ==
          """+-------+-------+-------+
            || _ _ _ | _ _ _ | _ _ _ |
            || _ _ _ | _ _ _ | _ _ _ |
            || _ _ _ | _ _ _ | _ _ _ |
            |+-------+-------+-------+
            || _ _ _ | _ _ _ | _ _ _ |
            || _ _ _ | _ _ _ | _ _ _ |
            || _ _ _ | _ _ _ | _ _ _ |
            |+-------+-------+-------+
            || _ _ _ | _ _ _ | _ _ _ |
            || _ _ _ | _ _ _ | _ _ _ |
            || _ _ _ | _ _ _ | _ _ _ |
            |+-------+-------+-------+""".stripMargin)
      }
    )
}
