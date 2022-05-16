package com.fq.sudoku

import cats.effect.{IO, Resource}
import cats.implicits.toTraverseOps
import com.fq.sudoku.Solver._
import fs2.Stream
import fs2.concurrent.Topic

object FS2StreamSolver extends Solver[IO] {
  override def solve(givens: List[Value.Given]): IO[List[Value]] =
    streamValues(givens).compile.toList

  def streamValues(givens: List[Value.Given]): Stream[IO, Value] =
    for {
      updatesTopic <- Stream.eval(Topic[IO, Value])
      givenCoords = givens.map(_.coord).toSet
      missingCoords = Coord.allCoords.filterNot(givenCoords.contains)
      givensStream = Stream.emits(givens)
      singleCandidateStreamsResource = missingCoords.traverse(deduceSingleCandidate(updatesTopic))
      singleCandidateStreams <- Stream.resource(singleCandidateStreamsResource)
      valuesStream = givensStream ++ singleCandidateStreams.reduce(_ merge _)
      values <- valuesStream.evalTap(updatesTopic.publish1)
    } yield values

  def deduceSingleCandidate(
      updatesTopic: Topic[IO, Value]
  )(coord: Coord): Resource[IO, Stream[IO, Candidate.Single]] =
    updatesTopic
      .subscribeAwait(100)
      .map { peerUpdatesStream =>
        peerUpdatesStream
          .filter(_.coord.isPeerOf(coord))
          .mapAccumulate[Candidate, Candidate](Candidate.initial(coord)) {
            case (multiple: Candidate.Multiple, peerValue) =>
              val nextCandidate = multiple.refine(peerValue)
              (nextCandidate, nextCandidate)
            case (single: Candidate.Single, _) => (single, single)
          }
          .collectFirst { case (_, single: Candidate.Single) => single }
      }
}
