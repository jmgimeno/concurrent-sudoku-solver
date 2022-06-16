package com.fq.sudoku

import zio.test._
import zio.test.Assertion._

import com.fq.sudoku.Solver.Coord.allCoords
import com.fq.sudoku.Solver.Value

object ZIOUtilsTest extends ZIOSpecDefault {

  def spec: Spec[Any, Nothing] =
    suite("Units Test")(
      test("boardToString") {
        assert(Solver.toString(allCoords.map(Value.Given(_, 1))))(
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
        assert(Solver.toString(List.empty))(
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
