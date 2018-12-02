package day1

import cats.effect._
import cats.implicits._

import fs2._
import fs2.io._

import java.nio.file.Paths

import scala.concurrent.ExecutionContext.global

case class FrequencyAccumulator(
  frequency: Int,
  frequenciesSeen: Set[Int],
  duplicateFound: Boolean)

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    args(0) match {
      case "part1" => part1
      case "part2" => part2
      case _       => IO.pure(ExitCode.Error)
    }
  }

  val input: Stream[IO, Int] =
    file.readAll[IO](Paths.get("src/main/scala/day1/input.txt"), global, 256)
      .through(text.utf8Decode)
      .through(text.lines)
      .collect { case s if s.nonEmpty => s.toInt }

  // Sum the ints in the input text file
  def part1: IO[ExitCode] = {
    input.reduceSemigroup
      .compile.last
      .flatMap(res => IO(println(res)))
      .as(ExitCode.Success)
  }

  // Keep a running frequency sum and an accumulating set of frequencies seen,
  // find the first duplicate running frequency while continually looping over the input file
  def part2: IO[ExitCode] = {
    input.repeat
      .zipWithScan(FrequencyAccumulator(0, Set(), false)) { case (acc, change) =>
        val newFrequency = acc.frequency + change
        val updatedFrequenciesSeen = acc.frequenciesSeen + newFrequency
        val duplicateFound = acc.frequenciesSeen.contains(newFrequency)

        FrequencyAccumulator(
          frequency = newFrequency,
          frequenciesSeen = updatedFrequenciesSeen,
          duplicateFound = duplicateFound)
      }
      .find { case (_, state) => state.duplicateFound }
      .compile.last
      .flatMap {
        case Some((_, result)) => IO(println(result.frequency)).as(ExitCode.Success)
        case None              => IO.pure(ExitCode.Error)
      }
  }
}
