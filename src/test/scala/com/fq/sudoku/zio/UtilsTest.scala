package com.fq.sudoku.zio

import com.fq.sudoku.zio.ZIOSolver.Coord._
import com.fq.sudoku.zio.ZIOSolver.Value
import zio.test._
import zio.test.Assertion._

object UtilsTest extends ZIOSpecDefault {

  def spec =
    suite("Units Test")(
      test("boardToString") {
        assert(ZIOSolver.toString(allCoords.map(Value.Given(_, 1))))(
          equalTo(
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
          |+-------+-------+-------+""".stripMargin
          )
        )
      },
      test("boardToString can handle missing values") {
        assert(ZIOSolver.toString(List.empty))(
          equalTo(
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
              |+-------+-------+-------+""".stripMargin
          )
        )
      }
    )
}
