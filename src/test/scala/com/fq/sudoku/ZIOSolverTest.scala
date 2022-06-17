package com.fq.sudoku

import zio._
import zio.test._
import zio.test.Assertion._

import com.fq.sudoku.Solver.{Candidate, Coord, Value}

object ZIOSolverTest extends ZIOSpecDefault {

  sealed trait SudokuValue {
    def value: Int
  }

  object SudokuValue {
    case class Given(value: Int) extends SudokuValue

    case class Missing(value: Int) extends SudokuValue
  }

  def toSudokuValue(value: Value): (Coord, SudokuValue) =
    value match {
      case Value.Given(coord, x)    => coord -> SudokuValue.Given(x)
      case single: Candidate.Single => single.coord -> SudokuValue.Missing(single.value)
    }

  // Easy puzzle
  val testCase1: Map[Coord, SudokuValue] = Map(
    Coord(0, 0) -> SudokuValue.Missing(2),
    Coord(0, 1) -> SudokuValue.Given(6),
    Coord(0, 2) -> SudokuValue.Missing(1),
    Coord(0, 3) -> SudokuValue.Given(3),
    Coord(0, 4) -> SudokuValue.Missing(7),
    Coord(0, 5) -> SudokuValue.Missing(5),
    Coord(0, 6) -> SudokuValue.Given(8),
    Coord(0, 7) -> SudokuValue.Missing(9),
    Coord(0, 8) -> SudokuValue.Given(4),
    Coord(1, 0) -> SudokuValue.Given(5),
    Coord(1, 1) -> SudokuValue.Given(3),
    Coord(1, 2) -> SudokuValue.Given(7),
    Coord(1, 3) -> SudokuValue.Missing(8),
    Coord(1, 4) -> SudokuValue.Given(9),
    Coord(1, 5) -> SudokuValue.Missing(4),
    Coord(1, 6) -> SudokuValue.Missing(1),
    Coord(1, 7) -> SudokuValue.Missing(6),
    Coord(1, 8) -> SudokuValue.Missing(2),
    Coord(2, 0) -> SudokuValue.Missing(9),
    Coord(2, 1) -> SudokuValue.Given(4),
    Coord(2, 2) -> SudokuValue.Missing(8),
    Coord(2, 3) -> SudokuValue.Missing(2),
    Coord(2, 4) -> SudokuValue.Missing(1),
    Coord(2, 5) -> SudokuValue.Given(6),
    Coord(2, 6) -> SudokuValue.Given(3),
    Coord(2, 7) -> SudokuValue.Missing(5),
    Coord(2, 8) -> SudokuValue.Given(7),
    Coord(3, 0) -> SudokuValue.Missing(6),
    Coord(3, 1) -> SudokuValue.Given(9),
    Coord(3, 2) -> SudokuValue.Missing(4),
    Coord(3, 3) -> SudokuValue.Missing(7),
    Coord(3, 4) -> SudokuValue.Given(5),
    Coord(3, 5) -> SudokuValue.Given(1),
    Coord(3, 6) -> SudokuValue.Given(2),
    Coord(3, 7) -> SudokuValue.Given(3),
    Coord(3, 8) -> SudokuValue.Given(8),
    Coord(4, 0) -> SudokuValue.Missing(8),
    Coord(4, 1) -> SudokuValue.Missing(2),
    Coord(4, 2) -> SudokuValue.Missing(5),
    Coord(4, 3) -> SudokuValue.Missing(9),
    Coord(4, 4) -> SudokuValue.Missing(4),
    Coord(4, 5) -> SudokuValue.Missing(3),
    Coord(4, 6) -> SudokuValue.Missing(6),
    Coord(4, 7) -> SudokuValue.Missing(7),
    Coord(4, 8) -> SudokuValue.Missing(1),
    Coord(5, 0) -> SudokuValue.Given(7),
    Coord(5, 1) -> SudokuValue.Given(1),
    Coord(5, 2) -> SudokuValue.Given(3),
    Coord(5, 3) -> SudokuValue.Given(6),
    Coord(5, 4) -> SudokuValue.Given(2),
    Coord(5, 5) -> SudokuValue.Missing(8),
    Coord(5, 6) -> SudokuValue.Missing(9),
    Coord(5, 7) -> SudokuValue.Given(4),
    Coord(5, 8) -> SudokuValue.Missing(5),
    Coord(6, 0) -> SudokuValue.Given(3),
    Coord(6, 1) -> SudokuValue.Missing(5),
    Coord(6, 2) -> SudokuValue.Given(6),
    Coord(6, 3) -> SudokuValue.Given(4),
    Coord(6, 4) -> SudokuValue.Missing(8),
    Coord(6, 5) -> SudokuValue.Missing(2),
    Coord(6, 6) -> SudokuValue.Missing(7),
    Coord(6, 7) -> SudokuValue.Given(1),
    Coord(6, 8) -> SudokuValue.Missing(9),
    Coord(7, 0) -> SudokuValue.Missing(4),
    Coord(7, 1) -> SudokuValue.Missing(8),
    Coord(7, 2) -> SudokuValue.Missing(9),
    Coord(7, 3) -> SudokuValue.Missing(1),
    Coord(7, 4) -> SudokuValue.Given(6),
    Coord(7, 5) -> SudokuValue.Missing(7),
    Coord(7, 6) -> SudokuValue.Given(5),
    Coord(7, 7) -> SudokuValue.Given(2),
    Coord(7, 8) -> SudokuValue.Given(3),
    Coord(8, 0) -> SudokuValue.Given(1),
    Coord(8, 1) -> SudokuValue.Missing(7),
    Coord(8, 2) -> SudokuValue.Given(2),
    Coord(8, 3) -> SudokuValue.Missing(5),
    Coord(8, 4) -> SudokuValue.Missing(3),
    Coord(8, 5) -> SudokuValue.Given(9),
    Coord(8, 6) -> SudokuValue.Missing(4),
    Coord(8, 7) -> SudokuValue.Given(8),
    Coord(8, 8) -> SudokuValue.Missing(6)
  )

  // Easy puzzle: https://sandiway.arizona.edu/sudoku/examples.html
  val testCase2: Map[Coord, SudokuValue] = Map(
    Coord(0, 0) -> SudokuValue.Missing(4),
    Coord(0, 1) -> SudokuValue.Missing(3),
    Coord(0, 2) -> SudokuValue.Missing(5),
    Coord(0, 3) -> SudokuValue.Given(2),
    Coord(0, 4) -> SudokuValue.Given(6),
    Coord(0, 5) -> SudokuValue.Missing(9),
    Coord(0, 6) -> SudokuValue.Given(7),
    Coord(0, 7) -> SudokuValue.Missing(8),
    Coord(0, 8) -> SudokuValue.Given(1),
    Coord(1, 0) -> SudokuValue.Given(6),
    Coord(1, 1) -> SudokuValue.Given(8),
    Coord(1, 2) -> SudokuValue.Missing(2),
    Coord(1, 3) -> SudokuValue.Missing(5),
    Coord(1, 4) -> SudokuValue.Given(7),
    Coord(1, 5) -> SudokuValue.Missing(1),
    Coord(1, 6) -> SudokuValue.Missing(4),
    Coord(1, 7) -> SudokuValue.Given(9),
    Coord(1, 8) -> SudokuValue.Missing(3),
    Coord(2, 0) -> SudokuValue.Given(1),
    Coord(2, 1) -> SudokuValue.Given(9),
    Coord(2, 2) -> SudokuValue.Missing(7),
    Coord(2, 3) -> SudokuValue.Missing(8),
    Coord(2, 4) -> SudokuValue.Missing(3),
    Coord(2, 5) -> SudokuValue.Given(4),
    Coord(2, 6) -> SudokuValue.Given(5),
    Coord(2, 7) -> SudokuValue.Missing(6),
    Coord(2, 8) -> SudokuValue.Missing(2),
    Coord(3, 0) -> SudokuValue.Given(8),
    Coord(3, 1) -> SudokuValue.Given(2),
    Coord(3, 2) -> SudokuValue.Missing(6),
    Coord(3, 3) -> SudokuValue.Given(1),
    Coord(3, 4) -> SudokuValue.Missing(9),
    Coord(3, 5) -> SudokuValue.Missing(5),
    Coord(3, 6) -> SudokuValue.Missing(3),
    Coord(3, 7) -> SudokuValue.Given(4),
    Coord(3, 8) -> SudokuValue.Missing(7),
    Coord(4, 0) -> SudokuValue.Missing(3),
    Coord(4, 1) -> SudokuValue.Missing(7),
    Coord(4, 2) -> SudokuValue.Given(4),
    Coord(4, 3) -> SudokuValue.Given(6),
    Coord(4, 4) -> SudokuValue.Missing(8),
    Coord(4, 5) -> SudokuValue.Given(2),
    Coord(4, 6) -> SudokuValue.Given(9),
    Coord(4, 7) -> SudokuValue.Missing(1),
    Coord(4, 8) -> SudokuValue.Missing(5),
    Coord(5, 0) -> SudokuValue.Missing(9),
    Coord(5, 1) -> SudokuValue.Given(5),
    Coord(5, 2) -> SudokuValue.Missing(1),
    Coord(5, 3) -> SudokuValue.Missing(7),
    Coord(5, 4) -> SudokuValue.Missing(4),
    Coord(5, 5) -> SudokuValue.Given(3),
    Coord(5, 6) -> SudokuValue.Missing(6),
    Coord(5, 7) -> SudokuValue.Given(2),
    Coord(5, 8) -> SudokuValue.Given(8),
    Coord(6, 0) -> SudokuValue.Missing(5),
    Coord(6, 1) -> SudokuValue.Missing(1),
    Coord(6, 2) -> SudokuValue.Given(9),
    Coord(6, 3) -> SudokuValue.Given(3),
    Coord(6, 4) -> SudokuValue.Missing(2),
    Coord(6, 5) -> SudokuValue.Missing(6),
    Coord(6, 6) -> SudokuValue.Missing(8),
    Coord(6, 7) -> SudokuValue.Given(7),
    Coord(6, 8) -> SudokuValue.Given(4),
    Coord(7, 0) -> SudokuValue.Missing(2),
    Coord(7, 1) -> SudokuValue.Given(4),
    Coord(7, 2) -> SudokuValue.Missing(8),
    Coord(7, 3) -> SudokuValue.Missing(9),
    Coord(7, 4) -> SudokuValue.Given(5),
    Coord(7, 5) -> SudokuValue.Missing(7),
    Coord(7, 6) -> SudokuValue.Missing(1),
    Coord(7, 7) -> SudokuValue.Given(3),
    Coord(7, 8) -> SudokuValue.Given(6),
    Coord(8, 0) -> SudokuValue.Given(7),
    Coord(8, 1) -> SudokuValue.Missing(6),
    Coord(8, 2) -> SudokuValue.Given(3),
    Coord(8, 3) -> SudokuValue.Missing(4),
    Coord(8, 4) -> SudokuValue.Given(1),
    Coord(8, 5) -> SudokuValue.Given(8),
    Coord(8, 6) -> SudokuValue.Missing(2),
    Coord(8, 7) -> SudokuValue.Missing(5),
    Coord(8, 8) -> SudokuValue.Missing(9)
  )

  // Easy puzzle: https://sandiway.arizona.edu/sudoku/examples.html
  val testCase3: Map[Coord, SudokuValue] = Map(
    Coord(0, 0) -> SudokuValue.Given(1),
    Coord(0, 1) -> SudokuValue.Missing(5),
    Coord(0, 2) -> SudokuValue.Missing(2),
    Coord(0, 3) -> SudokuValue.Given(4),
    Coord(0, 4) -> SudokuValue.Given(8),
    Coord(0, 5) -> SudokuValue.Given(9),
    Coord(0, 6) -> SudokuValue.Missing(3),
    Coord(0, 7) -> SudokuValue.Missing(7),
    Coord(0, 8) -> SudokuValue.Given(6),
    Coord(1, 0) -> SudokuValue.Given(7),
    Coord(1, 1) -> SudokuValue.Given(3),
    Coord(1, 2) -> SudokuValue.Missing(9),
    Coord(1, 3) -> SudokuValue.Missing(2),
    Coord(1, 4) -> SudokuValue.Missing(5),
    Coord(1, 5) -> SudokuValue.Missing(6),
    Coord(1, 6) -> SudokuValue.Missing(8),
    Coord(1, 7) -> SudokuValue.Given(4),
    Coord(1, 8) -> SudokuValue.Missing(1),
    Coord(2, 0) -> SudokuValue.Missing(4),
    Coord(2, 1) -> SudokuValue.Missing(6),
    Coord(2, 2) -> SudokuValue.Missing(8),
    Coord(2, 3) -> SudokuValue.Missing(3),
    Coord(2, 4) -> SudokuValue.Missing(7),
    Coord(2, 5) -> SudokuValue.Given(1),
    Coord(2, 6) -> SudokuValue.Given(2),
    Coord(2, 7) -> SudokuValue.Given(9),
    Coord(2, 8) -> SudokuValue.Given(5),
    Coord(3, 0) -> SudokuValue.Missing(3),
    Coord(3, 1) -> SudokuValue.Missing(8),
    Coord(3, 2) -> SudokuValue.Given(7),
    Coord(3, 3) -> SudokuValue.Given(1),
    Coord(3, 4) -> SudokuValue.Given(2),
    Coord(3, 5) -> SudokuValue.Missing(4),
    Coord(3, 6) -> SudokuValue.Given(6),
    Coord(3, 7) -> SudokuValue.Missing(5),
    Coord(3, 8) -> SudokuValue.Missing(9),
    Coord(4, 0) -> SudokuValue.Given(5),
    Coord(4, 1) -> SudokuValue.Missing(9),
    Coord(4, 2) -> SudokuValue.Missing(1),
    Coord(4, 3) -> SudokuValue.Given(7),
    Coord(4, 4) -> SudokuValue.Missing(6),
    Coord(4, 5) -> SudokuValue.Given(3),
    Coord(4, 6) -> SudokuValue.Missing(4),
    Coord(4, 7) -> SudokuValue.Missing(2),
    Coord(4, 8) -> SudokuValue.Given(8),
    Coord(5, 0) -> SudokuValue.Missing(2),
    Coord(5, 1) -> SudokuValue.Missing(4),
    Coord(5, 2) -> SudokuValue.Given(6),
    Coord(5, 3) -> SudokuValue.Missing(8),
    Coord(5, 4) -> SudokuValue.Given(9),
    Coord(5, 5) -> SudokuValue.Given(5),
    Coord(5, 6) -> SudokuValue.Given(7),
    Coord(5, 7) -> SudokuValue.Missing(1),
    Coord(5, 8) -> SudokuValue.Missing(3),
    Coord(6, 0) -> SudokuValue.Given(9),
    Coord(6, 1) -> SudokuValue.Given(1),
    Coord(6, 2) -> SudokuValue.Given(4),
    Coord(6, 3) -> SudokuValue.Given(6),
    Coord(6, 4) -> SudokuValue.Missing(3),
    Coord(6, 5) -> SudokuValue.Missing(7),
    Coord(6, 6) -> SudokuValue.Missing(5),
    Coord(6, 7) -> SudokuValue.Missing(8),
    Coord(6, 8) -> SudokuValue.Missing(2),
    Coord(7, 0) -> SudokuValue.Missing(6),
    Coord(7, 1) -> SudokuValue.Given(2),
    Coord(7, 2) -> SudokuValue.Missing(5),
    Coord(7, 3) -> SudokuValue.Missing(9),
    Coord(7, 4) -> SudokuValue.Missing(4),
    Coord(7, 5) -> SudokuValue.Missing(8),
    Coord(7, 6) -> SudokuValue.Missing(1),
    Coord(7, 7) -> SudokuValue.Given(3),
    Coord(7, 8) -> SudokuValue.Given(7),
    Coord(8, 0) -> SudokuValue.Given(8),
    Coord(8, 1) -> SudokuValue.Missing(7),
    Coord(8, 2) -> SudokuValue.Missing(3),
    Coord(8, 3) -> SudokuValue.Given(5),
    Coord(8, 4) -> SudokuValue.Given(1),
    Coord(8, 5) -> SudokuValue.Given(2),
    Coord(8, 6) -> SudokuValue.Missing(9),
    Coord(8, 7) -> SudokuValue.Missing(6),
    Coord(8, 8) -> SudokuValue.Given(4)
  )

  // Intermediate puzzle: https://sandiway.arizona.edu/sudoku/examples.html
  // Can't be solved with just single candidate technique
  val testCase4: Map[Coord, SudokuValue] = Map(
    Coord(0, 0) -> SudokuValue.Missing(1),
    Coord(0, 1) -> SudokuValue.Given(2),
    Coord(0, 2) -> SudokuValue.Missing(3),
    Coord(0, 3) -> SudokuValue.Given(6),
    Coord(0, 4) -> SudokuValue.Missing(7),
    Coord(0, 5) -> SudokuValue.Given(8),
    Coord(0, 6) -> SudokuValue.Missing(9),
    Coord(0, 7) -> SudokuValue.Missing(4),
    Coord(0, 8) -> SudokuValue.Missing(5),
    Coord(1, 0) -> SudokuValue.Given(5),
    Coord(1, 1) -> SudokuValue.Given(8),
    Coord(1, 2) -> SudokuValue.Missing(4),
    Coord(1, 3) -> SudokuValue.Missing(2),
    Coord(1, 4) -> SudokuValue.Missing(3),
    Coord(1, 5) -> SudokuValue.Given(9),
    Coord(1, 6) -> SudokuValue.Given(7),
    Coord(1, 7) -> SudokuValue.Missing(6),
    Coord(1, 8) -> SudokuValue.Missing(1),
    Coord(2, 0) -> SudokuValue.Missing(9),
    Coord(2, 1) -> SudokuValue.Missing(6),
    Coord(2, 2) -> SudokuValue.Missing(7),
    Coord(2, 3) -> SudokuValue.Missing(1),
    Coord(2, 4) -> SudokuValue.Given(4),
    Coord(2, 5) -> SudokuValue.Missing(5),
    Coord(2, 6) -> SudokuValue.Missing(3),
    Coord(2, 7) -> SudokuValue.Missing(2),
    Coord(2, 8) -> SudokuValue.Missing(8),
    Coord(3, 0) -> SudokuValue.Given(3),
    Coord(3, 1) -> SudokuValue.Given(7),
    Coord(3, 2) -> SudokuValue.Missing(2),
    Coord(3, 3) -> SudokuValue.Missing(4),
    Coord(3, 4) -> SudokuValue.Missing(6),
    Coord(3, 5) -> SudokuValue.Missing(1),
    Coord(3, 6) -> SudokuValue.Given(5),
    Coord(3, 7) -> SudokuValue.Missing(8),
    Coord(3, 8) -> SudokuValue.Missing(9),
    Coord(4, 0) -> SudokuValue.Given(6),
    Coord(4, 1) -> SudokuValue.Missing(9),
    Coord(4, 2) -> SudokuValue.Missing(1),
    Coord(4, 3) -> SudokuValue.Missing(5),
    Coord(4, 4) -> SudokuValue.Missing(8),
    Coord(4, 5) -> SudokuValue.Missing(3),
    Coord(4, 6) -> SudokuValue.Missing(2),
    Coord(4, 7) -> SudokuValue.Missing(7),
    Coord(4, 8) -> SudokuValue.Given(4),
    Coord(5, 0) -> SudokuValue.Missing(4),
    Coord(5, 1) -> SudokuValue.Missing(5),
    Coord(5, 2) -> SudokuValue.Given(8),
    Coord(5, 3) -> SudokuValue.Missing(7),
    Coord(5, 4) -> SudokuValue.Missing(9),
    Coord(5, 5) -> SudokuValue.Missing(2),
    Coord(5, 6) -> SudokuValue.Missing(6),
    Coord(5, 7) -> SudokuValue.Given(1),
    Coord(5, 8) -> SudokuValue.Given(3),
    Coord(6, 0) -> SudokuValue.Missing(8),
    Coord(6, 1) -> SudokuValue.Missing(3),
    Coord(6, 2) -> SudokuValue.Missing(6),
    Coord(6, 3) -> SudokuValue.Missing(9),
    Coord(6, 4) -> SudokuValue.Given(2),
    Coord(6, 5) -> SudokuValue.Missing(4),
    Coord(6, 6) -> SudokuValue.Missing(1),
    Coord(6, 7) -> SudokuValue.Missing(5),
    Coord(6, 8) -> SudokuValue.Missing(7),
    Coord(7, 0) -> SudokuValue.Missing(2),
    Coord(7, 1) -> SudokuValue.Missing(1),
    Coord(7, 2) -> SudokuValue.Given(9),
    Coord(7, 3) -> SudokuValue.Given(8),
    Coord(7, 4) -> SudokuValue.Missing(5),
    Coord(7, 5) -> SudokuValue.Missing(7),
    Coord(7, 6) -> SudokuValue.Missing(4),
    Coord(7, 7) -> SudokuValue.Given(3),
    Coord(7, 8) -> SudokuValue.Given(6),
    Coord(8, 0) -> SudokuValue.Missing(7),
    Coord(8, 1) -> SudokuValue.Missing(4),
    Coord(8, 2) -> SudokuValue.Missing(5),
    Coord(8, 3) -> SudokuValue.Given(3),
    Coord(8, 4) -> SudokuValue.Missing(1),
    Coord(8, 5) -> SudokuValue.Given(6),
    Coord(8, 6) -> SudokuValue.Missing(8),
    Coord(8, 7) -> SudokuValue.Missing(9),
    Coord(8, 8) -> SudokuValue.Given(2)
  )

  val testCases: Seq[Map[Coord, SudokuValue]] = Seq(testCase1) //, testCase2)//, testCase3)

  val solvers: List[Solver[Task]] =
    List(
      ZIOPromiseRefRaceSolver
    )

  def givens(expected: Map[Coord, SudokuValue]): List[Value.Given] =
    expected.collect {
      case (k, SudokuValue.Given(v)) => Value.Given(k, v)
    }.toList

  def spec: Spec[Any, Throwable] =
    suite("ZIOPromiseRefRaceSolver")(
      test("testCase1") {
        val givens = ZIOSolverTest.givens(testCase1)
        val result = ZIOPromiseRefRaceSolver.solve(givens)
        assertZIO(result.map(_.map(toSudokuValue).toMap))(equalTo(testCase1))
      },
      test("testCase2") {
        val givens = ZIOSolverTest.givens(testCase2)
        val result = ZIOPromiseRefRaceSolver.solve(givens)
        assertZIO(result.map(_.map(toSudokuValue).toMap))(equalTo(testCase2))
      },
      test("testCase3") {
        val givens = ZIOSolverTest.givens(testCase3)
        val result = ZIOPromiseRefRaceSolver.solve(givens)
        assertZIO(result.map(_.map(toSudokuValue).toMap))(equalTo(testCase3))
      }
    )
}
