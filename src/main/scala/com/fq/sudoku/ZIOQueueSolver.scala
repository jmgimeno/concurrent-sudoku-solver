package com.fq.sudoku

import zio._
import com.fq.sudoku.Solver._

object ZIOQueueSolver extends Solver[Task] {
  override def solve(givenValues: List[Value.Given]): Task[List[Value]] =
    for {
      givenCoords <- ZIO.succeed(givenValues.map(_.coord).toSet)
      missingCoords = Coord.allCoords.filterNot(givenCoords.contains)
      missingCells <- ZIO.foreach(missingCoords)(MissingCell.make)
      broadcast = broadcastToPeers(missingCells)(_)
      _ <- ZIO.foreachParDiscard(givenValues)(broadcast).withParallelismUnbounded
      missingValues <- ZIO.foreachPar(missingCells)(cell => cell.solve.tap(broadcast)).withParallelismUnbounded
    } yield givenValues ++ missingValues

  case class MissingCell(coord: Coord, updatesQueue: Queue[Value]) {
    val solve: Task[Candidate.Single] = refineToSingleCandidate(Candidate.initial(coord))

    private def refineToSingleCandidate(candidates: Candidate.Multiple): Task[Candidate.Single] =
      for {
        peerValue <- updatesQueue.take
        singleCandidate <- candidates.refine(peerValue) match {
          case single: Candidate.Single     => ZIO.succeed(single)
          case multiple: Candidate.Multiple => refineToSingleCandidate(multiple)
        }
      } yield singleCandidate
  }

  object MissingCell {
    def make(coord: Coord): Task[MissingCell] =
      Queue.unbounded[Value].map(queue => MissingCell(coord, queue))
  }

  def broadcastToPeers(cells: List[MissingCell])(update: Value): Task[Unit] =
    ZIO.foreachParDiscard(
      cells
        .filter(cell => cell.coord.isPeerOf(update.coord))
    )(cell => cell.updatesQueue.offer(update)).withParallelismUnbounded
}
