package day2

import cats.effect._
import cats.implicits._

import fs2._
import fs2.io._

import java.nio.file.Paths

import scala.concurrent.ExecutionContext.global

case class CheckString(hasExactly2: Boolean, hasExactly3: Boolean)

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    args(0) match {
      case "part1" => part1
      case "part2" => part2
      case _       => IO.pure(ExitCode.Error)
    }
  }

  val input: Stream[IO, String] =
    file.readAll[IO](Paths.get("src/main/scala/day2/input.txt"), global, 256)
      .through(text.utf8Decode)
      .through(text.lines)
      .filter(_.nonEmpty)

  def checkString(str: String): CheckString = {
    val counted = str.groupBy(identity)
      .mapValues(_.length)
      .map(_.swap)

    CheckString(
      hasExactly2 = counted.contains(2),
      hasExactly3 = counted.contains(3))
  }

  def part1 = input.map(checkString)
    .compile
    .fold((0, 0)) { case ((twos, threes), cs) =>
      val newTwos = twos + (if (cs.hasExactly2) 1 else 0)
      val newThrees = threes + (if (cs.hasExactly3) 1 else 0)

      (newTwos, newThrees)
    }
    .flatMap { case (totalTwos, totalThrees) => IO(println(totalTwos * totalThrees)) }
    .as(ExitCode.Success)

  def part2 = {
    input.compile.toList
      .map { list =>
        list.sorted.sliding(2)
          .find(pairs => pairs(0).diff(pairs(1)).length == 1)
      }
      .flatMap {
        case Some(pairs) => IO(println(pairs(0).intersect(pairs(1)))).as(ExitCode.Success)
        case None        => IO(ExitCode.Error)
      }
  }
}
