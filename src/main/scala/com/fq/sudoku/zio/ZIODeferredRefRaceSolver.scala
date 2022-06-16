package com.fq.sudoku.zio

import zio._
import com.fq.sudoku.zio.ZIOSolver._

object ZIODeferredRefRaceSolver extends ZIOSolver {
  def solve(givens: List[Value.Given]): Task[List[Value]] =
    for {
      allCells <- ZIO.foreach(Coord.allCoords)(Cell.make)
      givensMap = givens.map(g => g.coord -> g).toMap
      values <- ZIO.foreachPar(allCells)(_.solve(givensMap, allCells))
    } yield values

  trait Cell {
    def coord: Coord
    protected[this] def deferredValue: Promise[Nothing, Value]
    def getValue: Task[Value] = deferredValue.await
    def deduceSingleCandidate(allCells: List[Cell]): Task[Value]
    def solve(givensMap: Map[Coord, Value.Given], allCells: List[Cell]): Task[Value] =
      (givensMap.get(coord) match {
        case Some(givenValue) => ZIO.succeed(givenValue)
        case None             => deduceSingleCandidate(allCells)
      }).tap(deferredValue.succeed)
  }

  object Cell {
    def make(_coord: Coord): Task[Cell] =
      for {
        _deferredValue <- Promise.make[Nothing, Value]
      } yield new Cell {
        override val coord: Coord = _coord

        override val deferredValue: Promise[Nothing, Value] = _deferredValue

        override def deduceSingleCandidate(allCells: List[Cell]): Task[Candidate.Single] =
          for {
            refCandidate <- Ref.make[Candidate](Candidate.initial(coord))
            peerCells = allCells.filter(cell => cell.coord.isPeerOf(coord))
            listOfSingleCandidateOrNever =
              peerCells.map(peerCell => refineToSingleCandidateOrNever(refCandidate, peerCell))
            singleCandidate <- raceMany(listOfSingleCandidateOrNever)
          } yield singleCandidate

        private def raceMany[T](listOfIOs: List[Task[T]]): Task[T] =
          listOfIOs.reduce((a, b) => a.raceEither(b).map(_.merge))

        private def refineToSingleCandidateOrNever(
            refCandidate: Ref[Candidate],
            peerCell: Cell
        ): Task[Candidate.Single] =
          for {
            peerValue <- peerCell.getValue
            singleCandidate <- refCandidate.modify {
              case multiple: Candidate.Multiple =>
                multiple.refine(peerValue) match {
                  case single: Candidate.Single     => (ZIO.succeed(single), single)
                  case multiple: Candidate.Multiple => (ZIO.never, multiple)
                }
              case alreadySingle: Candidate.Single => (ZIO.never, alreadySingle)
            }.flatten
          } yield singleCandidate
      }
  }
}
